(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defparameter *max-save-attempts* 100
  "Maximum number of times to try to save a message before giving up")

(defvar *max-message-size* 10000
  "The maximum number of characters in a single message.")

(defclass message-updated ()
  ((date         :type local-time:timestamp
                 :initarg :date
                 :reader message-updated/date
                 :documentation "The timestamp of the most recent update of this message.")
   (text         :type (or null string)
                 :initarg :text
                 :reader message-updated/text
                 :documentation "The content of the most recent version of this message.")
   (extra-html   :type (or null string)
                 :initarg :extra-html
                 :reader message-updated/extra-html
                 :documentation "Extra HTML content that will be added to the end of the message content.")
   (image        :type list
                 :initarg :image
                 :initform nil
                 :reader message-updated/image
                 :documentation "The image-link content of the most recent version of this message")
   (deleted      :type (or null (eql t))
                 :initarg :deleted
                 :initform nil
                 :reader message-updated/deleted
                 :documentation "Whether the current state of the message is deleted"))
  (:documentation "Information about a modification of a message"))

(defclass message ()
  ((id            :type (or null string)
                  :initarg :id
                  :initform nil
                  :accessor message/id
                  :documentation "Message id, or nil if the message has not been saved")
   (created-date  :type local-time:timestamp
                  :initform (local-time:now)
                  :initarg :created-date
                  :reader message/created-date
                  :documentation "The timestamp when the message was created")
   (updated-date  :type (or null local-time:timestamp)
                  :initform nil
                  :initarg :updated-date
                  :reader message/updated-date
                  :documentation "The timestamp when the message was last updated")
   (from          :type string
                  :initarg :from
                  :reader message/from
                  :documentation "The ID of the user that sent the message")
   (from-name     :type string
                  :initarg :from-name
                  :reader message/from-name
                  :documentation "The name of the user who sent the message")
   (channel       :type string
                  :initarg :channel
                  :reader message/channel
                  :documentation "The channel the message was sent to")
   (text          :type string
                  :initarg :text
                  :reader message/text
                  :documentation "The message text")
   (extra-html    :type (or null string)
                  :initarg :extra-html
                  :initform nil
                  :reader message/extra-html
                  :documentation "Supplementary HTML content in this message")
   (image         :type list
                  :initarg :image
                  :initform nil
                  :reader message/image
                  :documentation "The s3 key pointing to the attached image, if one exists")
   (updated       :type list
                  :initarg :updated
                  :initform nil
                  :reader message/updated
                  :documentation "Record of changes made to this message")
   (deleted       :type (or null (eql t))
                  :initarg :deleted
                  :initform nil
                  :reader message/deleted
                  :documentation "Indicate if an update marks a deletion of a message")
   (file-list     :type list
                  :initarg :file-list
                  :initform nil
                  :reader message/file-list
                  :documentation "If this message is a file upload, a list of files.")
   (star-users    :type list
                  :initarg :star-users
                  :initform nil
                  :reader message/star-users
                  :documentation "List of id's of users that have starred this message.")
   (hidden-users  :type list
                  :initarg :hidden-users
                  :initform nil
                  :reader message/hidden-users
                  :documentation "List of id's of users who have hidden this message."))
  (:documentation "Content of a single chat message"))

(defmethod print-object ((obj message) stream)
  (print-unreadable-safely (id created-date from channel text) obj stream
    (format stream "~s FROM ~s CHANNEL ~s TEXT ~s" id from channel text)))

(defgeneric make-message (channel user text &key files extra-html)
  (:method ((channel string) user text &key files extra-html)
    (make-message (potato.db:load-instance 'channel channel) user text :files files :extra-html extra-html))
  (:method (channel (user string) text &key files extra-html)
    (make-message channel (load-user user) text :files files :extra-html extra-html))
  (:method ((channel channel) (user user) text &key files extra-html)
    (make-instance 'message
                   :from (user/id user)
                   :from-name (user/description user)
                   :channel (channel/id channel)
                   :text text
                   :extra-html extra-html
                   :file-list files)))

(declaim (inline ensure-message))
(defun ensure-message (message)
  (etypecase message
    (message message)
    (string (load-message message))))

(defun valid-image-thumbnail-descriptor-p (image)
  (or (null image)
      (stringp image)
      (and (listp image)
           (stringp (first image))
           (integerp (second image))
           (integerp (third image)))))

(defun message-modification-is-allowed-for-user (message user)
  (when (equal (message/from message) (ensure-user-id user))
    ;; Fast check that avoids loading the user and channel objects if
    ;; the user is the original author.
    (return-from message-modification-is-allowed-for-user t))
  ;; We need to check whether the user is admin in the group that the
  ;; channel is in.
  (let ((user (potato.core:ensure-user user))
        (channel (potato.db:load-instance 'potato.core:channel (potato.core:message/channel message))))
    (potato.core:user-is-admin-in-group-p (potato.core:channel/group channel) user)))

(defun save-message-modification (message user text extra-html image deleted-p)
  (unless (valid-image-thumbnail-descriptor-p image)
    (error "Illegal image thumbnail format: ~a" image))
  (let* ((message-obj (etypecase message
                        (string (load-message message))
                        (message message)))
         (message-id (message/id message-obj))
         (now (local-time:now)))
    (unless (message-modification-is-allowed-for-user message-obj user)
      (raise-permission-error "Not allowed to modify message"))
    (with-messages-db
      (potato.db:call-clouchdb-update-function "channel" "update_message_text" message-id
                                               `(("date"                . ,(format-timestamp nil now))
                                                 ("update_message_text" . ,(or text ""))
                                                 ("update_extra_html"   . ,(or extra-html ""))
                                                 ("update_image"        . ,(if (listp image) (car image) image))
                                                 ("update_image_width"  . ,(if (listp image) (princ-to-string (second image))))
                                                 ("update_image_height" . ,(if (listp image) (princ-to-string (third image))))
                                                 ("update_deleted_p"    . ,(if deleted-p "1" "0"))))
      (let ((m (load-message message-id)))
        (send-message-update-to-rabbitmq-queue m)))))

(defun make-message-id-no-index (channel date)
  (let ((channel-id (ensure-channel-id channel)))
    (format nil "msg-~a-~a" (encode-name channel-id) date)))

(defun image-descriptor->couch (desc)
  (append (list (cons :|file| (first desc)))
          (if (cdr desc)
              (list (cons :|width| (second desc))
                    (cons :|height| (third desc))))))

(defun image-couch->descriptor (data)
  (when data
    (let ((file (getfield :|file| data))
          (width (getfield :|width| data :accept-missing t))
          (height (getfield :|height| data :accept-missing t)))
      (if (and width height)
          (list file width height)
          (list file)))))

(defun save-message (m)
  (when (message/id m)
    (error "Message has already been saved: ~s" m))
  (let* ((channel-id (message/channel m))
         (formatted-date (format-timestamp nil (message/created-date m)))
         (idname (make-message-id-no-index (message/channel m) formatted-date)))

    (labels ((make-file-list (files)
               (mapcar #'(lambda (v) `((:|key| . ,(first v))
                                       (:|name| . ,(second v))
                                       (:|size| . ,(third v))))
                       files))

             (attempt-save (index)
               (handler-case
                   (let ((image (message/image m))
                         (files (message/file-list m))
                         (deleted (message/deleted m))
                         (star-users (message/star-users m))
                         (hidden-users (message/hidden-users m)))
                     (with-messages-db
                       (clouchdb:create-document `((:|type|         . "message")
                                                   (:|created_date| . ,formatted-date)
                                                   (:|from|         . ,(message/from m))
                                                   (:|from_name|    . ,(message/from-name m))
                                                   (:|channel|      . ,(message/channel m))
                                                   (:|text|         . ,(message/text m))
                                                   (:|extra_html|   . ,(message/extra-html m))
                                                   ,@(if image (list (cons :|image| (image-descriptor->couch image))))
                                                   ,@(if files (list (cons :|file_list| (make-file-list files))))
                                                   ,@(if deleted (list (cons :|deleted| t)))
                                                   ,@(if star-users (list (cons :|star_users| star-users)))
                                                   ,@(if hidden-users (list (cons :|hidden| hidden-users))))
                                                 :id (format nil "~a_~5,'0d" idname index))))
                 (clouchdb:id-or-revision-conflict (condition)
                   (log:warn "Error saving document: ~s, index: ~s" condition index)
                   nil))))

      (let ((doc (loop
                    for index from 0
                    for wait-time = 1/10 then (max (+ wait-time 1/50) 1)
                    for doc = (attempt-save index)
                    repeat *max-save-attempts*
                    until doc
                    do (sleep wait-time)
                    finally (progn
                              (unless doc
                                (error "Unable to save message after ~a attempts" *max-save-attempts*))
                              (return doc)))))
        (setf (message/id m) (getfield :|id| doc))
        (send-message-update-to-rabbitmq-queue m)
        ;; Handle the post-submission processing in the background.
        ;; There is no need to let the client wait for this to
        ;; finish.
        (run-future
          (let ((mapping (potato.db:load-instance 'channel-users (channel-users-mapping-name channel-id))))
            (update-unread-state-after-post-message mapping)
            (potato.user-notification:process-user-notifications-for-new-message m mapping)))
        (log:trace "Message saved: ~s" (getfield :|id| doc))
        doc))))

(defun make-message-from-couchdb (result)
  (labels ((make-file-descriptor (content)
             (list (getfield :|key| content)
                   (getfield :|name| content)
                   (getfield :|size| content)))

           (make-message-updated-from-couchdb (content)
             (loop
                for m in content
                collect (make-instance 'message-updated
                                       :date (parse-timestamp (getfield :|updated_date| m))
                                       :text (getfield :|text| m)
                                       :image (getfield :|image| m :accept-missing t)
                                       :extra-html (getfield :|extra_html| m :accept-missing t)
                                       :deleted (if (getfield :|deleted| m :accept-missing t) t nil)))))

    (let ((updated-date (getfield :|updated_date| result :accept-missing t)))
      (make-instance 'message
                     :id            (getfield :|_id| result)
                     :created-date  (parse-timestamp (getfield :|created_date| result))
                     :updated-date  (if updated-date (parse-timestamp updated-date) nil)
                     :from          (getfield :|from| result)
                     :from-name     (getfield :|from_name| result)
                     :channel       (getfield :|channel| result)
                     :text          (getfield :|text| result)
                     :extra-html    (getfield :|extra_html| result :accept-missing t)
                     :image         (image-couch->descriptor (getfield :|image| result :accept-missing t))
                     :updated       (make-message-updated-from-couchdb (cdr (assoc :|update| result)))
                     :deleted       (if (getfield :|deleted| result :accept-missing t) t nil)
                     :file-list     (mapcar #'make-file-descriptor (getfield :|file_list| result :accept-missing t))
                     :star-users    (getfield :|star_users| result :accept-missing t)
                     :hidden-users  (getfield :|hidden| result :accept-missing t)))))

(defun load-message (id)
  (with-messages-db
    (let ((result (clouchdb:get-document id)))
      (make-message-from-couchdb result))))

(defun load-message-with-check (message-id user)
  ;; The naive way of checking the permission here would be to
  ;; simply load the message, and then use LOAD-CHANNEL-WITH-CHECK
  ;; to ensure that the user has access to the message. However,
  ;; this would result in different errors being raised depending
  ;; on whether the message-id exists or not, which can be a
  ;; security problem. Instead, we check everything manually and
  ;; send the same error regardless of error.
  (labels ((raise-message-error ()
             (raise-permission-error "Illegal message")))
    (let ((msg (load-message message-id)))
      (unless msg
        (raise-message-error))
      (handler-case
          (let ((channel (load-channel-with-check (message/channel msg) :user user :if-not-joined :load)))
            (unless channel
              (raise-message-error))
            msg)
        (permission-error () (raise-message-error))))))

(defun load-message-log (channel num-messages start)
  (check-type channel (or channel string))
  (check-type num-messages integer)
  (check-type start (or null string))
  (with-messages-db
    (let* ((channel-id (ensure-channel-id channel))
           (result (clouchdb:invoke-view "channel" "created_date"
                                         :start-key (list channel-id (or start 'clouchdb:json-map))
                                         :end-key (list channel-id nil)
                                         :descending t
                                         :limit num-messages)))
      (let ((rows (getfield :|rows| result)))
        (mapcar #'(lambda (v)
                    (make-message-from-couchdb (getfield :|value| v)))
                (reverse (if start (cdr rows) rows)))))))

(defun message-updated->json (m formatter-fn)
  (st-json:jso "updated_date" (format-timestamp nil (message-updated/date m))
               "text" (funcall formatter-fn (message-updated/text m))
               "extra_html" (or (message-updated/extra-html m) :null)
               "deleted" (st-json:as-json-bool (message-updated/deleted m))))

(defun find-message-cd-converter-by-name (converter-name)
  (ecase converter-name
    (:text #'identity)
    (:html #'(lambda (msg) (format-message msg)))
    (:alist #'message-content-as-json)))

(defun message-cd->json-with-converter (v type original-content recipient-user)
  (let* ((formatter-fn (find-message-cd-converter-by-name type))
         (id (message/id v))
         (from-id (message/from v))
         (created-date (message/created-date v))
         (text (message/text v))
         (extra-html (message/extra-html v))
         (image (message/image v))
         (deleted-p (message/deleted v))
         (formatted (funcall formatter-fn text)))
    (apply #'st-json:jso
           "id" id
           "created_date" (format-timestamp nil created-date)
           "channel" (message/channel v)
           "from" from-id
           "from_name" (message/from-name v)
           "text" (if deleted-p "" formatted)
           "use_math" (if (message-contains-math-p text) :true :false)
           "hash" (let ((content-utf (babel:string-to-octets (concatenate 'string from-id "_" text) :encoding :utf-8)))
                    (ironclad:byte-array-to-hex-string (ironclad:digest-sequence 'ironclad:sha1 content-utf)))
           (append (if extra-html
                       (list "extra_html" extra-html))
                   (if image
                       (list "image" (apply #'st-json:jso
                                            "file" (format nil "/download/~a" (first image))
                                            (if (= (length image) 3)
                                                (list "width" (second image)
                                                      "height" (third image))))))
                   (alexandria:if-let ((files (message/file-list v)))
                     (list "files" (mapcar #'(lambda (v)
                                               (destructuring-bind (key name size) v
                                                 (st-json:jso "key" key
                                                              "name" name
                                                              "size" size
                                                              "location" (potato.upload:make-download-location key))))
                                           files)))
                   (alexandria:if-let ((star-users (message/star-users v)))
                     (if recipient-user
                         (let ((recipient-uid (ensure-user-id recipient-user)))
                           (if (member recipient-uid star-users :test #'equal)
                               (list "star_users" (list recipient-uid))))
                         (list "star_users" star-users)))
                   (alexandria:if-let ((hidden-users (message/hidden-users v)))
                     (if recipient-user
                         (let ((recipient-uid (ensure-user-id recipient-user)))
                           (if (member recipient-uid hidden-users :test #'equal)
                               (list "hidden_users" (list recipient-uid))))
                         (list "hidden_users" hidden-users)))
                   (if deleted-p
                       (list "deleted" (if deleted-p t :json-false)))
                   (alexandria:if-let ((updated (message/updated v)))
                     (list "updated_date" (format-timestamp nil (message/updated-date v))
                           "updated" (if original-content
                                         (mapcar (lambda (m) (message-updated->json m formatter-fn)) updated)
                                         (length updated))))))))

(defun message-cd->json (v type recipient-user)
  (message-cd->json-with-converter v type nil recipient-user))

(defun message-detailed->json (message type recipient-user)
  "Format the full message content as JSON, including the full update history."
  (message-cd->json-with-converter message type t recipient-user))

(defun update-message-star (msg user add-p)
  (let ((uid (ensure-user-id user))
        (cid (message/channel msg))
        (msgid (message/id msg)))
    (with-messages-db
      (potato.db:call-clouchdb-update-function "channel" "update_star_user" msgid
                                               `(("user_id" . ,uid)
                                                 ("add" . ,(if add-p "1" "0")))))
    (with-pooled-rabbitmq-connection (conn)
      (cl-rabbit:basic-publish conn 1
                               :exchange *channel-exchange-name*
                               :routing-key (format nil "update-star.~a" (encode-name-for-routing-key cid))
                               :body (lisp-to-binary (list :update-star cid msgid  uid (if add-p t nil)))))))

(defun update-message-star-with-check (msgid add-p &key user)
  (let ((user (or user (current-user))))
    (let ((msg (load-message-with-check msgid user)))
      (update-message-star msg user add-p ))))

(defun update-message-hidden (msg user add-p)
  (let ((uid (ensure-user-id user))
        (cid (message/channel msg))
        (msgid (message/id msg)))
    (with-messages-db
      (potato.db:call-clouchdb-update-function "channel" "update_hidden_user" msgid
                                               `(("user_id" . ,uid)
                                                 ("add" . ,(if add-p "1" "0")))))
    (with-pooled-rabbitmq-connection (conn)
      (cl-rabbit:basic-publish conn 1
                               :exchange *channel-exchange-name*
                               :routing-key (format nil "update-hidden.~a" (encode-name-for-routing-key cid))
                               :body (lisp-to-binary (list :update-hidden cid msgid  uid (if add-p t nil)))))))

(defun update-message-hidden-with-check (msgid add-p &key user)
  (let ((user (or user (current-user))))
    (let ((msg (load-message-with-check msgid user)))
      (update-message-hidden msg user add-p ))))

(defun load-message-range (message &optional (num-rows 5))
  (check-type message message)
  (with-messages-db
    (let* ((channel-id (message/channel message))
           (id (list channel-id (message/id message)))
           (l1 (clouchdb:invoke-view "channel" "created_date"
                                     :start-key id
                                     :end-key (list channel-id nil)
                                     :limit num-rows
                                     :descending t))
           (l2 (clouchdb:invoke-view "channel" "created_date"
                                     :start-key id
                                     :end-key (list channel-id 'clouchdb:json-map)
                                     :limit num-rows)))
      (let* ((trailing (cdr (getfield :|rows| l2)))
             (range (mapcar (lambda (v) (getfield :|value| v)) (append (reverse (getfield :|rows| l1)) trailing))))
        (mapcar (lambda (v) (make-message-from-couchdb v)) range)))))

(defun load-message-range-with-check (message-id &optional (num-rows 5))
  (with-messages-db
    (let* ((message (load-message message-id)))
      (load-channel-with-check (message/channel message) :if-not-joined :load)
      (load-message-range message num-rows))))

(defun send-message-update-to-rabbitmq-queue (message)
  (with-pooled-rabbitmq-connection (conn)
    (cl-rabbit:basic-publish conn 1
                             :exchange *channel-content-exchange-name*
                             :routing-key (message/channel message)
                             :properties (list (cons :correlation-id (message/id message)))
                             :body (conspack:encode message))))
