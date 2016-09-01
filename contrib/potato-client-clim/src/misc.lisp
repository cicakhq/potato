(in-package :potato-client-clim)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun parse-timestamp (s)
  (local-time:parse-timestring s))

(defun format-timestamp (stream ts)
  (local-time:format-timestring stream ts
                                :format (append local-time:+iso-8601-date-format+
                                                          '("T")
                                                          local-time:+iso-8601-time-format+
                                                          '("Z"))
                                :timezone local-time:+utc-zone+))

(defun nil-if-json-null (v)
  (if (eq v :null)
      nil
      v))

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

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))
