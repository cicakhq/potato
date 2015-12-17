(defpackage :potato.db-fixes
  (:use :cl :potato.common)
  (:export #:fix-images
           #:transfer-messages))

(in-package :potato.db-fixes)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun fix-images ()
  "Change raw file ID's in the image element to a hash so that it can
hold more information (for example the image's dimensions)."
  (labels ((process-doc (v)
             (when (equal (cdr (assoc :|type| v)) "message")
                                       (let ((active-image (assoc :|active_image| (cdr (assoc :|updated| v))))
                                             (image (assoc :|image| v)))
                                         (cond ((and active-image (stringp (cdr active-image)))
                                                (setf (cdr active-image) `((:|file| . ,(cdr active-image))))
                                                (format t "Updating root message = ~s~%" (cdr (assoc :|_id| v)))
                                                (clouchdb:create-document v))
                                               ((and image (stringp (cdr image)))
                                                (setf (cdr image) `((:|file| . ,(cdr image))))
                                                (format t "Updating update message = ~s~%" (cdr (assoc :|_id| v)))
                                                (clouchdb:create-document v)))))))
    (potato.db::call-with-db-entries #'process-doc)))

(defun mkstring (key alist)
  (let ((v (getfield key alist :accept-missing t)))
    (or v "")))

(defun mkint (key alist)
  (let ((v (getfield key alist :accept-missing t)))
    (if v
        (progn
          (check-type v alexandria:positive-integer)
          (princ-to-string v))
        "")))

(defun process-update-document (doc)
  (restart-case
      (when (equal (cdr (assoc :|type| doc)) "message")
        (let ((msgid (cdr (assoc :|_id| doc))))
          (alexandria:when-let ((u (cdr (assoc :|updated| doc))))
            (clouchdb:put-document (remove :|updated| doc :key #'car) :id msgid)
            (loop
               for mid in (getfield :|updated_by| u)
               for updated-doc = (clouchdb:get-document mid)
               for img = (getfield :|image| updated-doc :accept-missing t)
               for deleted-p = (getfield :|update_delete| updated-doc :accept-missing t)
               do (potato.db:call-clouchdb-update-function "channel" "update_message_text" msgid
                                                           `(("date" . ,(getfield :|created_date| updated-doc))
                                                             ("update_message_text" . ,(if deleted-p
                                                                                           ""
                                                                                           (mkstring :|text| updated-doc)))
                                                             ("updated_extra_html" . ,(if deleted-p
                                                                                          ""
                                                                                          (mkstring :|extra_html| updated-doc)))
                                                             ("update_image" . ,(if (and (not deleted-p) img)
                                                                                    (getfield :|file| img)))
                                                             ("update_image_width"  . ,(if (and (not deleted-p) img)
                                                                                           (mkint :|width| img)))
                                                             ("update_image_height" . ,(if (and (not deleted-p) img)
                                                                                           (mkint :|height| img)))
                                                             ("update_deleted_p"    . ,(if deleted-p "1" "0"))))))))
    (skip-document ()
      :report "Skip document" nil)))

(defun remove-update-document (doc)
  (when (equal (cdr (assoc :|type| doc)) "message")
    (let ((update (cdr (assoc :|update| doc))))
      (when (stringp update)
        (let ((id (getfield :|_id| doc)))
          (format t "Deleting ~a~%" id)
          (clouchdb:delete-document id))))))

(defun update-modifications ()
  (with-messages-db
    (potato.db::call-with-db-entries #'process-update-document)
    (potato.db::call-with-db-entries #'remove-update-document)))

(defun transfer-image-thumbnails ()
  (potato.common.application:start-component 'potato.common::generic)
  (potato.common.application:start-component 'potato.db::db)
  (format t "Updating thumbnails~%")
  (let ((result (with-messages-db
                  (clouchdb:ad-hoc-view "function(doc) { if(doc.type === 'message' && doc.image && doc.image.file.indexOf('/') != -1) { emit(doc._id, doc); } }"))))
    (dolist (row (getfield :|rows| result))
      (let ((msgid (getfield :|key| row))
            (msg (getfield :|value| row)))
        (format t "Converting ~s~%" msgid)
        (let* ((key (getfield :|file| (getfield :|image| msg)))
               (cid (getfield :|channel| msg))
               (uid (getfield :|from| msg))
               (head-result (zs3:head :bucket *s3-bucket* :key key))
               (now (local-time:now)))
          (let ((file (make-instance 'potato.upload:file
                                     :name (subseq key (1+ (position #\/ key)))
                                     :channel cid
                                     :key key
                                     :user uid
                                     :message "Thumbnail image"
                                     :mime-type (getfield :content-type head-result)
                                     :location :s3)))
            (setf (potato.upload::file/size file) (parse-integer (getfield :content-length head-result)))
            (setf (potato.upload::file/verify-date file) now)
            (setf (potato.upload::file/confirmed-p file) now)
            (potato.db:save-instance file)
            (setf (getfield :|file| (getfield :|image| msg)) (potato.upload:file/id file))
            (with-messages-db
              (clouchdb:put-document msg)))))))
  ;; Update links to files
  (format t "Updating attachments~%")
  (with-messages-db
    (let ((result (clouchdb:ad-hoc-view "function(doc) {
                                             if(doc.type === 'message' && doc.file_list) {
                                                 for(var i = 0 ; i < doc.file_list.length ; i++ ) {
                                                     if(doc.file_list[i].key.indexOf('/') != -1) {
                                                         emit(doc._id,doc);
                                                         return; } } } }")))
      (dolist (row (getfield :|rows| result))
        (let ((msg (getfield :|value| row)))
          (format t "Converting message ~s~%" (getfield :|key| row))
          (setf (getfield :|file_list| msg)
                (mapcar (lambda (v)
                          (cons (cons :|key| (concatenate 'string "file-" (encode-name (getfield :|key| v))))
                                (remove :|key| v :key #'car)))
                        (getfield :|file_list| msg)))
          (clouchdb:put-document msg))))))
 
