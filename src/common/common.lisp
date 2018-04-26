(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(defvar *solr-path-location* "http://127.0.0.1:8983/solr/potato")
(defvar *potato-standalone* nil)

(alexandria:define-constant +BLANK-CHARS+ (map 'string #'identity '(#\Space #\Newline #\Return)) :test 'equal)

(potato.common.application:define-component solr
  (:start
   (setq cl-solr:*solr-url-path* *solr-path-location*)))

(potato.common.application:define-component lparallel
  (:start
   (setf lparallel:*kernel* (lparallel:make-kernel 10))))

(defmacro print-unreadable-safely ((&rest slots) object stream &body body)
  "A version of PRINT-UNREADABLE-OBJECT and WITH-SLOTS that is safe to use with unbound slots"
  (let ((object-copy (gensym "OBJECT"))
        (stream-copy (gensym "STREAM")))
    `(let ((,object-copy ,object)
           (,stream-copy ,stream))
       (symbol-macrolet ,(mapcar (lambda (slot-name)
                                   `(,slot-name (if (and (slot-exists-p ,object-copy ',slot-name)
                                                         (slot-boundp ,object-copy ',slot-name))
                                                    (slot-value ,object-copy ',slot-name)
                                                    :not-bound)))
                                 slots)
         (print-unreadable-object (,object-copy ,stream-copy :type t :identity nil)
           ,@body)))))

(defun encode-name (string)
  (check-type string string)
  (with-output-to-string (s)
    (loop
       for ch across string
       for code = (char-code ch)
       if (or (<= (char-code #\a) code (char-code #\z))
              (<= (char-code #\A) code (char-code #\Z))
              (<= (char-code #\0) code (char-code #\9))
              (eql ch #\@)
              (eql ch #\_)
              (eql ch #\.)
              (eql ch #\,)
              (> code 255))
       do (princ ch s)
       else
       do (format s "!~2,'0x" code))))

(defun encode-name-for-routing-key (user-id)
  "Old-style user id's can have a period in them. RabbitMQ uses a
period as field separator for the routing keys, so this function is
needed to santise the names before using them in a routing key."
  ;; First, we check for the normal case; no period in the user name
  (let ((pos (position #\. user-id)))
    (if pos
        (cl-ppcre:regex-replace-all "\\." user-id "-")
        ;; ELSE: No period in the user name, simly return the user-id unchanged.
        user-id)))

(defun decode-name (string)
  (check-type string string)
  (with-output-to-string (s)
    (loop
       with len = (length string)
       with i = 0
       while (< i len)
       for ch = (aref string i)
       do (incf i)
       if (eq ch #\!)
       do (progn
            (princ (code-char (parse-integer string :start i :end (+ 2 i) :radix 16)) s)
            (incf i 2))
       else do (princ ch s))))

(defun call-with-logged-conditions (name fn)
  (flet ((backtrace-as-string ()
           (with-output-to-string (out)
             (trivial-backtrace:print-backtrace-to-stream out))))
    (handler-bind ((error (lambda (condition)
                            (log:error "~a: ~a. Backtrace: ~a" name condition (backtrace-as-string))))
                   (warning (lambda (condition)
                              (log:warn "~a: ~a. Backtrace: ~a" name condition (backtrace-as-string)))))
      (funcall fn))))

(defun start-monitored-thread (fn name)
  (check-type fn function)
  (check-type name string)
  (flet ((run-simple ()
           (unwind-protect
                (funcall fn)
             (log:error "Thread terminated: ~a" name)))
         ;;
         (run-with-monitor ()
           (loop
              for index from 0
              do (block monitor-thread
                   (handler-bind ((error (lambda (condition)
                                           (let ((backtrace (with-output-to-string (s)
                                                              (trivial-backtrace:print-backtrace-to-stream s))))
                                             (log:error "Unhandled error in monitored thread. Error: ~a~%~a"
                                                        condition backtrace)
                                             (return-from monitor-thread nil)))))
                     (funcall fn)))
              ;; Sleep for one second before restarting the thread
              do (sleep 1))))
    ;;
    (if *debug*
        (bordeaux-threads:make-thread #'run-simple :name (format nil "Simple server thread: ~a" name))
        ;; ELSE: Not debug mode, restart the thread after it stops
        (bordeaux-threads:make-thread #'run-with-monitor :name (format nil "Monitored thread: ~a" name)))))

(declaim (inline getfield))
(defun getfield (key result &key accept-missing)
  (let ((v (assoc key result)))
    (if v
        (cdr v)
        (if accept-missing
            nil
            (error "Missing value for: ~s, in: ~s" key result)))))

(defun (setf getfield) (value key result)
  (let ((v (assoc key result)))
    (unless v
      (error "Missing value for: ~s" key))
    (setf (cdr v) value)))

(defun value-by-xpath (expression node &key (default-value nil default-value-assigned-p))
  (let ((result (xpath:evaluate expression node)))
    (if (xpath:node-set-empty-p result)
        (if default-value-assigned-p
            default-value
            (error "No value found for expression: ~s" expression))
        (dom:node-value (xpath:first-node result)))))

(defun empty-string-or-nil-p (v)
  (check-type v (or null string))
  (or (null v) (string= v "")))

(declaim (inline string-start-match))
(defun string-start-match (key string)
  (and (>= (length string) (length key))
       (string= key string :end2 (length key))))

(defun lisp-to-binary (v)
  (babel:string-to-octets (prin1-to-string v) :encoding :utf-8))

(defun binary-to-lisp (v)
  (let ((*read-eval* nil))
    (values (read-from-string (babel:octets-to-string v :encoding :utf-8)))))

(defun string->sha1 (string)
  (let* ((bytes (babel:string-to-octets string :encoding :utf-8))
         (digest-bytes (ironclad:digest-sequence 'ironclad:sha1 bytes)))
    (ironclad:byte-array-to-hex-string digest-bytes)))

(defun %markup-from-regexp (regexp string callback &optional plain-string-markup-fn)
  (flet ((markup-string (s)
           (if plain-string-markup-fn
               (funcall plain-string-markup-fn s)
               (list s))))
    (loop
       with length = (length string)
       with start = 0
       while (< start length)
       append (multiple-value-bind (match-start match-end reg-starts reg-ends)
                  (cl-ppcre:scan regexp string :start start)
                (if match-start
                    ;; Some highlighted text was found
                    (let* ((highlight (funcall callback reg-starts reg-ends))
                           (old-start start))
                      (setq start match-end)
                      (if (> match-start old-start)
                          ;; There is some unmatched text before the match
                          (append (markup-string (subseq string old-start match-start))
                                  (list highlight))
                          ;; ELSE: The match is at the beginning of the string
                          (list highlight)))
                    ;; ELSE: No match, copy the last part of the text and finish the loop
                    (progn
                      (let ((old-start start))
                        (setq start length)
                        (markup-string (subseq string old-start)))))))))

(defmacro markup-from-regexp (regexp string callback &optional plain-string-markup-fn &environment env)
  `(%markup-from-regexp ,(if (constantp regexp env) `(load-time-value (cl-ppcre:create-scanner ,regexp)) regexp)
                        ,string ,callback ,@(if plain-string-markup-fn (list plain-string-markup-fn))))

(defun escape-string (string stream)
  (loop
     for c across string
     do (case c
          (#\& (write-string "&amp;" stream))
          (#\< (write-string "&lt;" stream))
          (#\> (write-string "&gt;" stream))
          (t   (write-char c stream)))))

(declaim (inline ensure-printable-string))
(defun ensure-printable-string (string)
  "Check that STRING contains valid characters and return STRING. If
it contains invalid characters, raise an error."
  (check-type string simple-string)
  (let ((charfilter (load-time-value (loop
                                        with len = 256
                                        with res = (make-array len :element-type 'bit)
                                        for i from 0 below len
                                        do (setf (aref res i)
                                                 (if (and (or (<= 0 i #x1f)
                                                              (<= #x7f i #x9f))
                                                          (/= i (char-code #\Newline))
                                                          (/= i (char-code #\Tab)))
                                                     1 0))
                                        finally (return res)))))
    (declare (type (array bit (256)) charfilter))
    (labels ((control-p (ch)
               (let ((code (char-code ch)))
                 (and (< code 256)
                      (not (zerop (aref charfilter code)))))))
      (let ((index (loop
                      for ch across string
                      when (control-p ch)
                      return t
                      finally (return nil))))
        (if index
            (with-output-to-string (s)
              (loop
                 for ch across string
                 when (not (control-p ch))
                 do (write-char ch s)))
            ;; ELSE: The string does not contain any control character, simply return it unchanged
            string)))))

(defun current-time ()
  (let ((now (local-time:now)))
    (+ (local-time:timestamp-to-unix now)
       (/ (local-time:nsec-of now) 1000000000))))

(defmacro recover-if-fail (form &body unwind-forms)
  (if unwind-forms
      (let ((success-sym (gensym "SUCCESS-")))
        `(let ((,success-sym nil))
           (unwind-protect
                (recover-if-fail
                    (prog1
                      ,form
                      (setq ,success-sym t))
                  ,@(butlast unwind-forms))
             (unless ,success-sym
               ,(car (last unwind-forms))))))
      ;; ELSE: No unwind forms, just return the main form.
      form))

(defun nil-if-json-null (v)
  (if (eq v :null)
      nil
      v))

(defmacro json-bind (definitions data &body body)
  (alexandria:with-gensyms (data-sym value-sym value-set-sym)
    `(let ((,data-sym ,data))
       (let ,(loop
               for row in definitions
               collect (destructuring-bind (sym name &key (required t) (type :default))
                           (if (symbolp row) (list row (string-downcase (symbol-name row))) row)
                         `(,sym ,`(multiple-value-bind (,value-sym ,value-set-sym)
                                      (st-json:getjso ,name ,data-sym)
                                    (if (and ,required (not ,value-set-sym))
                                        (error "Key ~s was not available in JSON data" ,name)
                                        ,(ecase type
                                           (:default `(nil-if-json-null ,value-sym))
                                           (:boolean `(if ,value-set-sym (st-json:from-json-bool ,value-sym) nil))))))))
         ,@body))))

(defmacro with-memcached-warnings-muffled (&body body)
  `(handler-bind ((pooler::pool-warning (lambda (condition)
                                          (declare (ignore condition))
                                          (invoke-restart 'muffle-warning))))
     ,@body))

(defvar *init-called* nil)

(potato.common.application:define-component generic
  (:dependencies logging lparallel rabbitmq clouchdb solr s3 potato.common.memcached::memcached)
  (:start
   (local-time:reread-timezone-repository)
   (setq *init-called* t)))
