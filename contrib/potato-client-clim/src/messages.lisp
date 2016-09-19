(in-package :potato-client-clim)

(defparameter *code-colour* (clim:make-rgb-color 0.3 0.7 0.3))
(defparameter *code-shadow* (clim:make-rgb-color 0.5 0.5 0.5))
(defparameter *code-border* (clim:make-rgb-color 0.88 0.894 0.894))
(defparameter *url-colour* (clim:make-rgb-color 0.2 0.2 1.0))
(defparameter *code-background* (clim:make-rgb-color 0.988 0.988 0.988))
(defparameter *code-padding* 2)
(defparameter *user-background* (clim:make-rgb-color 0.9 0.9 0.9))

(defclass message ()
  ((id           :type string
                 :initarg :id
                 :reader message/id)
   (channel      :type channel
                 :initarg :channel
                 :reader message/channel)
   (from         :type string
                 :initarg :from
                 :reader message/from)
   (from-name    :type string
                 :initarg :from-name
                 :reader message/from-name)
   (created-date :type local-time
                 :initarg :created-date
                 :reader message/created-date)
   (text         :type t
                 :initarg :text
                 :reader message/text)
   (deleted      :type t
                 :initarg :deleted
                 :reader message/deleted)
   (from-image   :initform nil
                 :accessor message/from-image)
   (update-index :type integer
                 :initform 0
                 :accessor message/update-index)))

(defmethod print-object ((obj message) stream)
  (print-unreadable-safely (id text) obj stream
    (format stream "ID ~s TEXT ~s" id text)))

(defun message/cache-key (message)
  (check-type message message)
  (log:trace "Getting cache key for ~s" (message/id message))
  (format nil "~a_~a" (message/id message) (message/update-index message)))

(defun make-message-from-json (channel msg)
  (let ((cid (st-json:getjso "channel" msg)))
    (unless (equal (channel/id channel) cid)
      (error "Attempt to create a message for incorrenct channel"))
    (let* ((from (st-json:getjso "from" msg))
           (message (make-instance 'message
                                   :id (st-json:getjso "id" msg)
                                   :channel channel
                                   :from from
                                   :from-name (st-json:getjso "from_name" msg)
                                   :created-date (parse-timestamp (st-json:getjso "created_date" msg))
                                   :text (parse-text-content channel (st-json:getjso "text" msg))
                                   :deleted (eq (st-json:getjso "deleted" msg) :true))))
      (let ((sender (find-user (channel/users channel) from)))
        (find-image-from-url (potato-frame/image-cache (channel/frame channel)) sender
                             (lambda (entry)
                               (setf (message/from-image message) (image-cache-entry/pixmap entry))))
        message))))

(defclass channel-content-view (clim:view)
  ())

(defclass set-element ()
  ((elements :type list
             :initarg :elements
             :reader set-element/elements)))

(defmethod print-object ((obj set-element) stream)
  (print-unreadable-safely (elements) obj stream
    (format stream "~s" elements)))

(defclass text-element ()
  ())

(defclass formatted-element (text-element)
  ((text :type t
         :initarg :text
         :reader formatted-element/text)))

(defmethod print-object ((obj formatted-element) stream)
  (print-unreadable-safely (text) obj stream
    (format stream "TEXT ~s" text)))

(defclass paragraph-element (formatted-element) ())
(defclass bold-element (formatted-element) ())
(defclass italics-element (formatted-element) ())
(defclass code-element (formatted-element) ())
(defclass newline-element (text-element) ())

(defclass code-block-element (text-element)
  ((language :type (or null string)
             :initarg :language
             :reader code-block-element/language)
   (code     :type string
             :initarg :code
             :reader code-block-element/code)))

(defclass url-element (text-element)
  ((addr        :type string
                :initarg :addr
                :reader url-element/addr)
   (description :type string
                :initarg :description
                :reader url-element/description)))

(defun parse-text-content (channel content)
  (etypecase content
    (string content)
    (list (make-instance 'set-element :elements (mapcar (lambda (v) (parse-text-content channel v)) content)))
    (st-json:jso (parse-text-part channel content))))

(defun make-url-element (content)
  (let ((addr (st-json:getjso "addr" content)))
    (make-instance 'url-element
                   :addr addr
                   :description (or (nil-if-json-null (st-json:getjso "description" content)) addr))))

(defun make-user-element (channel content)
  (log:info "Content: ~s" content)
  (find-user (channel/users channel) (st-json:getjso "user_id" content)))

(defun parse-text-part (channel content)
  (let ((type (st-json:getjso "type" content)))
    (labels ((make-element (name)
               (make-instance name :text (parse-text-content channel (st-json:getjso "e" content)))))
      (string-case:string-case (type)
        ("p" (make-element 'paragraph-element))
        ("b" (make-element 'bold-element))
        ("i" (make-element 'italics-element))
        ("code" (make-element 'code-element))
        ("newline" (make-instance 'newline-element))
        ("code-block" (make-instance 'code-block-element
                                     :language (nil-if-json-null (st-json:getjso "language" content))
                                     :code (st-json:getjso "code" content)))
        ("url" (make-url-element content))
        ("user" (make-user-element channel content))
        (t (format nil "[unknown type:~a]" type))))))

(clim:define-presentation-method clim:present (obj (type message) stream (view channel-content-view) &key)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
        (alexandria:when-let ((image (message/from-image obj)))
          (clim:draw-pattern* stream image 0 0)))
      (clim:formatting-cell (stream)
        (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
          (format stream "~a" (message/from-name obj)))
        (format stream " (~a)~%"
                (local-time:format-timestring nil (message/created-date obj)
                                              :format '(:day " " :short-month " " :year " " :hour ":" :min)))
        (clim:present (message/text obj))))))

(clim:define-presentation-method clim:present (obj (type formatted-element) stream (view channel-content-view) &key)
  (present-to-stream (formatted-element/text obj) stream))

(clim:define-presentation-method clim:present (obj (type bold-element) stream (view channel-content-view) &key)
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (present-to-stream (formatted-element/text obj) stream)))

(clim:define-presentation-method clim:present (obj (type italics-element) stream (view channel-content-view) &key)
  (clim:with-text-style (stream (clim:make-text-style nil :italic nil))
    (present-to-stream (formatted-element/text obj) stream)))

(clim:define-presentation-method clim:present (obj (type code-element) stream (view channel-content-view) &key)
  (let ((delta (+ *code-padding* 1)))
    (clim:stream-increment-cursor-position stream 0 delta)
    (clim:surrounding-output-with-border (stream :ink *code-border*
                                                 :background *code-background*
                                                 :move-cursor nil
                                                 :padding *code-padding*)
      (clim:with-text-style (stream (clim:make-text-style :fix nil nil))
        (clim:with-drawing-options (stream :ink *code-colour*)
          (let ((v (formatted-element/text obj)))
            (clim:present v (clim:presentation-type-of v) :stream stream)))))
    (clim:stream-increment-cursor-position stream 0 (- delta))))

(clim:define-presentation-method clim:present (obj (type paragraph-element) stream (view channel-content-view) &key)
  (clim:present (formatted-element/text obj))
  (format stream "~%"))

(clim:define-presentation-method clim:present (obj (type code-block-element) stream (view channel-content-view) &key)
  (format stream "~&")
  (clim:surrounding-output-with-border (stream :ink *code-border*
                                               :background *code-background*)
    (clim:with-text-style (stream (clim:make-text-style :fix nil nil))
      (clim:with-drawing-options (stream :ink *code-colour*)
        (let ((v (code-block-element/code obj)))
          (clim:present v (clim:presentation-type-of v) :stream stream)))))
  (format stream "~&"))

(clim:define-presentation-method clim:present (obj (type string) stream (view channel-content-view) &key)
  (format stream "~a" obj))

(clim:define-presentation-method clim:present (obj (type set-element) stream (view channel-content-view) &key)
  (dolist (element (set-element/elements obj))
    (present-to-stream element stream)))

(clim:define-presentation-method clim:present (obj (type url-element) stream (view channel-content-view) &key)
  (clim:with-drawing-options (stream :ink *url-colour*)
    (clim:surrounding-output-with-border (stream :shape :underline)
      (format stream "~a" (url-element/description obj)))))

(clim:define-presentation-method clim:present (obj (type user) stream (view channel-content-view) &key)
  (clim:surrounding-output-with-border (stream :background *user-background* :padding 2)
   (format stream "~a" (user/description obj))))
