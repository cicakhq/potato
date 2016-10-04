(in-package :potato-index)

(declaim #.potato.common::*compile-decl*)

(defvar *channel-group-cache* (receptacle:make-blocking-hash-map :name "Map from channel to group" :test #'equal))
(defvar *main-seq-field-name* "couchdb_seq")
(defvar *main-seq* nil)

(defvar *message-seq-field-name* "couchdb_message_seq")
(defvar *message-seq* nil)

(defstruct cached-channel channel-name group-id group-name)

(defun ensure-int-string (value)
  (etypecase value
    (string value)
    (integer (princ-to-string value))))

(defun get-current-main-seq ()
  (or *main-seq*
      (let* ((result (cl-solr:query-noparse (concatenate 'string "id:" *main-seq-field-name*)))
             (db-seq (value-by-xpath "/response/result[@name='response']/doc/str[@name='current']/text()" result
                                     :default-value "0")))
        (setq *main-seq* db-seq)
        db-seq)))

(defun get-current-message-seq ()
  (or *message-seq*
      (let* ((result (cl-solr:query-noparse (concatenate 'string "id:" *message-seq-field-name*)))
             (db-seq (value-by-xpath "/response/result[@name='response']/doc/str[@name='current']/text()" result
                                     :default-value "0")))
        (setq *message-seq* db-seq)
        db-seq)))

(defun find-cached-channel (channel-id)
  (receptacle:hash-get-or-update *channel-group-cache* channel-id
                                 (lambda ()
                                   (let* ((channel-result (clouchdb:get-document channel-id))
                                          (group-id (getfield :|group| channel-result)))
                                     (let ((group-result (clouchdb:get-document group-id)))
                                       (make-cached-channel :group-id group-id
                                                            :channel-name (getfield :|name| channel-result)
                                                            :group-name (getfield :|name| group-result)))))))

(defvar *num-imports* 0)

(defun make-seq-update-document (field newseq)
  `(("id"      . ,field)
    ("current" . ,newseq)))

(defun send-update-to-index (solr-document seq-field newseq)
  (log:trace "Inserting document into index: ~s" solr-document)
  (incf *num-imports*)
  (handler-case
      (cl-solr:update-data (list solr-document (make-seq-update-document seq-field newseq))
                           :soft-commit (not (zerop (mod *num-imports* 1000))))
    (cl-solr:solr-response-error (condition)
      (log:error "Document could not be indexed. code=~s, reason=~s. Document content: ~s"
                 (cl-solr:solr-response-error/code condition)
                 (cl-solr:solr-response-error/text condition)
                 solr-document)
      (unless (= (cl-solr:solr-response-error/code condition) hunchentoot:+http-bad-request+)
        (error "Solr raised an error when trying to send document. code=~s, reason=~s."
               (cl-solr:solr-response-error/code condition)
               (cl-solr:solr-response-error/text condition))))))

(defun make-message-star (doc)
  (mapcar (lambda (v)
            (cons "star_user" v))
          (getfield :|star_users| doc :accept-missing t)))

(defun make-files (doc)
  (loop
    for file in (getfield :|file_list| doc :accept-missing t)
    append `(("attachment_name" . ,(getfield :|name| file))
             ("attachment_size" . ,(princ-to-string (getfield :|size| file))))))

(defun insert-doc-into-index (doc seq-field seq)
  (let* ((message-id (getfield :|_id| doc)))
    (if (cdr (assoc :|deleted| doc))
        (progn
          ;; TODO: Make sure this delete call works
          (cl-solr:delete-data (format nil "id:\"~a\"" message-id))
          (cl-solr:update-data (list (make-seq-update-document seq-field seq))))
        ;; ELSE: Normal message update
        (let* ((channel-id (getfield :|channel| doc))
               (channel (find-cached-channel channel-id))
               (html (format-message (getfield :|text| doc)))
               (message-star (make-message-star doc))
               (files (make-files doc))
               (solr-document `(("id"           . ,message-id)
                                ("potato_type"  . "message")
                                ("channel_id"   . ,channel-id)
                                ("channel_name" . ,(cached-channel-channel-name channel) )
                                ("created_date" . ,(getfield :|created_date| doc))
                                ("sender_id"    . ,(getfield :|from| doc))
                                ("sender_name"  . ,(getfield :|from_name| doc))
                                ("group_id"     . ,(cached-channel-group-id channel))
                                ("group_name"   . ,(cached-channel-group-name channel))
                                ("content"      . ,html)
                                ,@message-star
                                ,@files)))
          (send-update-to-index solr-document seq-field seq)))))

(defun insert-user-into-index (user-doc seq-field seq)
  (let* ((user-id (getfield :|_id| user-doc))
         (user-description (getfield :|description| user-doc))
         (domains-result (clouchdb:invoke-view "domain" "domains_for_user" :key user-id))
         (domains-rows (getfield :|rows| domains-result)))
    (if (null domains-rows)
        (log:warn "User ~a does not have any domains, not indexing: ~a" user-id seq)
        ;; ELSE: The user is in one or more domains
        (let ((solr-document `(("id" . ,user-id)
                               ("potato_type" . "user")
                               ("user_description" . ,user-description)
                               ;;("user_email" . ,user-email)
                               ("user_domain" . ,(mapcar (lambda (v) (getfield :|domain| (getfield :|value| v))) domains-rows)))))
          (send-update-to-index solr-document seq-field seq)))))

(defun update-memberdomain (doc seq-field seq)
  (handler-case
      (let ((result (clouchdb:get-document (getfield :|user| doc))))
        (insert-user-into-index result seq-field seq))
    (clouchdb:document-missing () (log:error "Update memberdomain for missing user. Memberdomain: ~s" (getfield :|_id| doc)))))

(defun do-main-couchdb-change (update)
  (log:trace "Index update on main db: ~s" update)
  (alexandria:when-let ((doc (cdr (assoc :|doc| update))))
    (let ((seq (ensure-int-string (getfield :|seq| update)))
          (type (cdr (assoc :|type| doc))))
      (when type
        (string-case:string-case (type)
          ("user" (insert-user-into-index doc *main-seq-field-name* seq))
          ("memberdomain" (update-memberdomain doc *main-seq-field-name* seq))))
      (setq *main-seq* seq))))

(defun do-message-couchdb-change (update)
  (log:trace "Index update on message db: ~s" update)
  (with-main-db
    (alexandria:when-let ((doc (cdr (assoc :|doc| update))))
      (let ((seq (ensure-int-string (getfield :|seq| update)))
            (type (cdr (assoc :|type| doc))))
        (when type
          (string-case:string-case (type)
            ("message" (insert-doc-into-index doc *message-seq-field-name* seq))))
        (setq *message-seq* seq)))))

(defun main-index-import-listener ()
  (loop
     do (clouchdb:changes :feed :continuous
                          :filter "index_filter/index_updates"
                          :notify-fn #'do-main-couchdb-change
                          :since (get-current-main-seq)
                          :include-docs t)))

(defun message-index-import-listener ()
  (with-messages-db
    (loop
       do (clouchdb:changes :feed :continuous
                            :filter "messagefilters/message_index_updates"
                            :notify-fn #'do-message-couchdb-change
                            :since (get-current-message-seq)
                            :include-docs t))))

(defun server-avail-p ()
  (handler-case
      (equal (cl-solr:ping-solr-server) "OK")
    (usocket:connection-refused-error () nil)))

(potato.common.application:define-component index-manager
  (:dependencies potato.common::generic potato.common::solr potato.common::clouchdb)
  (:start
   (if (server-avail-p)
       (progn
         ;; The 200 ms sleep before starting the threads are needed in
         ;; order to deal with an SBCL bug that causes a "Failed AVER"
         ;; when these two threads are started at the same time.
         (sleep 2/10)
         (start-monitored-thread #'main-index-import-listener "Main index importer")
         (sleep 2/10)
         (start-monitored-thread #'message-index-import-listener "Message index importer")
         (log:info "Index manager started"))
       ;; ELSE: The solr server is not available, print a warning message
       (log:warn "Solr server not available, index server not started"))))
