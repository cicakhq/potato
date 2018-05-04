(in-package :potato.views)

(declaim #.potato.common::*compile-decl*)

(defmacro ps-view-fix (fields &body body)
  `(remove #\Newline (ps-view ,fields ,@body)))

(defun create (id &rest view-defs)
  (let ((result (apply #'create-ps-view id view-defs)))
    (unless (cdr (assoc :|ok| result))
      (error "Error creating view ~a: ~s" id result))))

;;; This function is a lot more complicated that it needs to be.
;;; Originally we found some weird behaviour where the views were not
;;; created correctly even though the CouchDB calls returns ok. In
;;; order to analyse this, a lot of code was added to check that
;;; everything worked. Later, it was revealed that this was caused by
;;; a known CouchDB bug:
;;; https://issues.apache.org/jira/browse/COUCHDB-1415
;;;
;;; Thus, the :|created_date| field was added, but the old code that
;;; checks for correct ciew creation still remains until we can
;;; confirm that this was the cause of the issue.
(defun mkview (group-name view-defs &optional update-defs)
  (let ((id (concatenate 'string "_design/" group-name)))
    (loop
       for exists-p = (progn
                        (delete-document id :if-missing :ignore)
                        (get-document id :if-missing :ignore))
       while exists-p
       do (progn
            (log:error "View ~s still exists after deleting, waiting 1 second" id)
            (sleep 1)))
    (let ((result (create-document `((:|language| . "javascript")
                                     (:|views| . ,(loop
                                                     for (name map reduce) in view-defs
                                                     collect `(,name . ((:|map| . ,map)
                                                                        ,@(when reduce
                                                                            `((:|reduce| . ,reduce)))))))
                                     (:|created_date| . ,(potato.core:format-timestamp nil (local-time:now)))
                                     ,@(when update-defs
                                         `((:|updates| . ,(loop
                                                             for (name script) in update-defs
                                                             collect (cons name script))))))
                                   :id id)))
      (log:debug "Created view ~a. Result: ~s" group-name result)
      (sleep 1/10)
      ;; Verify that the views are available. This shouldn't be
      ;; needed, but in some circumstances we have seen that when
      ;; multiple views are created in quick succession, some views
      ;; are not created even though the creation succeded. The
      ;; purpose of this code is to detect this case so that it can be
      ;; debugged later.
      (let* ((result (get-document id))
             (result-views (potato.common:getfield :|views| result)))
        (loop
           for view in view-defs
           unless (potato.common:getfield (car view) result-views)
           do (error "View ~s missing after updating views" (car view)))))))

(defmacro with-zero-gensym (&body body)
  (setq *ps-gensym-counter* 0)
  `(progn ,@body))

(defmacro zs (&body body)
  `(with-zero-gensym
     (ps ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun docname (type)
    (potato.db:couchdb-type-for-class type)))

(defun init-potato-views ()
  (mkview "channel"
          `((:|users_in_channel|
              ,(zs (lambda (doc)
                     (with-slots (type channel users) doc
                       (when (and (eql type #.(docname 'potato.core:channel-users))
                                  (not (null users)))
                         (dolist (user-id ((@ |Object| keys) users))
                           (emit channel (create _id user-id))))))))
            (:|channels_for_group|
              ,(zs (lambda (doc)
                     (with-slots (type group) doc
                       (when (eql type #.(docname 'potato.core:channel))
                         (emit group doc))))))
            (:|recent_message|
              ,(zs (lambda (doc)
                     (with-slots (type channel users) doc
                       (when (and (eql type #.(docname 'potato.core:channel-users))
                                  (not (null users)))
                         (dolist (user-id ((@ |Object| keys) users))
                           (let ((entry (getprop users user-id)))
                             (when (> (@ entry count) 0)
                               (emit (list user-id channel) entry)))))))))
            (:|private_channel_list|
              ,(zs (lambda (doc)
                     (with-slots (type domain users group_type) doc
                       (when (and (eql type #.(docname 'potato.core:channel-users))
                                  (eql group_type "PRIVATE"))
                         (dolist (user-id ((@ |Object| keys) users))
                           (emit (list domain user-id) doc)))))))
            (:|channel_nickname|
              ,(zs (lambda (doc)
                     (with-slots (type channel) doc
                       (when (eql type #.(docname 'potato.core:channel-nickname))
                         (emit channel doc))))))
            (:|channels_in_domain|
              ,(zs (lambda (doc)
                     (with-slots (type domain) doc
                       (when (eql type #.(docname 'potato.core:channel))
                         (emit domain doc)))))))

          ;; Update functions
          `((:|add_user_to_channel|
              ,(zs (lambda (doc req)
                     (with-slots (type users deleted) doc
                       (cond
                         ((not (eql type #.(docname 'potato.core:channel-users)))
                          (throw "Illegal object type"))
                         (deleted
                          (list doc "\"Can't add users to a deleted channel\""))
                         (t
                          (let* ((user-id (@ req query user_id))
                                 (current-time (@ req query current_date))
                                 (new-mapping (create :|count| 0 :|last_read| current-time)))
                            (when (null users)
                              (setf users (create)))
                            (unless (getprop users user-id)
                              (setf (getprop users user-id) new-mapping))
                            (list doc "\"Updated\""))))))))
            (:|remove_user_from_channel|
              ,(zs (lambda (doc req)
                     (with-slots (type users group_type) doc
                       (unless (eql type #.(docname 'potato.core:channel-users))
                         (throw "Illegal object type"))
                       (let ((user-id (@ req query user_id)))
                         (cond ((eql group_type "PRIVATE")
                                (list doc (concatenate 'string "\"Channel is private\"")))
                               ((and users (getprop users user-id))
                                (delete (getprop users user-id))
                                (list doc (concatenate 'string "\"User " user-id " was removed from channel\"")))
                               (t
                                (list doc (concatenate 'string "\"User " user-id " is not a member of channel\"")))))))))
            (:|incmsg|
              ,(zs (lambda (doc req)
                     (with-slots (users) doc
                       (let ((ret ""))
                         (dolist (key ((@ |Object| keys) users))
                           (let ((u (getprop users key)))
                             (incf (getprop u "count") )
                             (when (not (eql ret ""))
                               (setf ret (concatenate 'string ret " ")))
                             (setf ret (concatenate 'string ret key))))
                         (list doc (concatenate 'string "\"" ret "\"")))))))
            (:|mark_read|
              ,(zs (lambda (doc req)
                     (with-slots (users) doc
                       (let ((user-id (@ req query user_id))
                             (date (@ req query date)))
                         (let* ((u (getprop users user-id))
                                old-count)
                           (if u
                               (progn
                                 (setf old-count (getprop u "count"))
                                 (setf (getprop u "count") 0)
                                 (setf (getprop u "last_read") date))
                               ;; ELSE: User does not exist
                               (setf old-count 0))
                           (list (if (> old-count 0) doc nil)
                                 (concatenate 'string "{\"count\":" old-count "}"))))))))
            (:|mark_hidden|
              ,(zs (lambda (doc req)
                     (with-slots (users) doc
                       (let* ((uid (@ req query user_id))
                              (show (eql (@ req query show) "1"))
                              (u (getprop users uid)))
                         (if u
                             (let ((old-hidden (getprop u "hide")))
                               (if (or (and old-hidden (not show))
                                       (and (not old-hidden) show))
                                   (list nil "\"ok\"")
                                   (progn
                                     (setf (getprop u "hide") (not show))
                                     (list doc "\"ok\""))))
                             ;; ELSE: User is not a member of channel
                             (list nil "\"not-member\"")))))))))

  (mkview "user"
          `((:|users_by_email|
              ,(zs (lambda (doc)
                     (with-slots (type email user) doc
                       (when (eql type #.(docname 'potato.core:user-email))
                         (emit email user))))))
            (:|emails_by_user|
              ,(zs (lambda (doc)
                     (with-slots (type email user) doc
                       (when (eql type #.(docname 'potato.core:user-email))
                         (emit user email))))))
            (:|channels_by_domain_and_user|
              ,(zs (lambda (doc)
                     (with-slots (type users channel domain name group group_type) doc
                       (when (eql type #.(docname 'potato.core:channel-users))
                         (let ((user-ids ((@ |Object| keys) users)))
                           (dolist (user-id user-ids)
                             (let ((user (getprop users user-id)))
                               (emit (list user-id domain)
                                     (list channel
                                           name
                                           (if (@ user hide) true false)
                                           group
                                           group_type
                                           (@ user count)
                                           (if (eql group_type "PRIVATE")
                                               (let ((uid0 (aref user-ids 0))
                                                     (uid1 (aref user-ids 1)))
                                                 (if (eql user-id uid0) uid1 uid0))
                                               ;; ELSE: This is not a private chat, return nil
                                               nil)))))))))))
            #+nil(:|user_description|
                   ,(zs (lambda (doc)
                          (with-slots (type _id description image_name default_image_name) doc
                            (when (eql type #.(docname 'potato.core:user))
                              (emit _id
                                    (list description (if (and image_name (not (eql image_name "")))
                                                          image_name
                                                          default_image_name)))))))))
          `((:|update_image_name|
              ,(zs (lambda (doc req)
                     (let ((name (@ req query name)))
                       (setf (getprop doc "image_name") name)
                       (list doc "\"ok\"")))))))

  (mkview "domain"
          `((:|users_in_domain|
              ,(zs (lambda (doc)
                     (with-slots (type domain user user_name role) doc
                       (when (eql type #.(docname 'potato.core:domain-user))
                         (emit domain (create :|user| user
                                              :|user_name| user_name
                                              :|role| role)))))))
            (:|user_count_in_domain|
              ,(zs (lambda (doc)
                     (with-slots (type domain) doc
                       (when (eql type #.(docname 'potato.core:domain-user))
                         (emit domain 1)))))
              ,(zs (lambda (key values rereduce)
                     (sum values))))
            (:|domains_for_user|
              ,(zs (lambda (doc)
                     (with-slots (type domain domain_name user role) doc
                       (when (eql type #.(docname 'potato.core:domain-user))
                         (emit user (create :|domain| domain
                                            :|domain_name| domain_name
                                            :|role| role)))))))
            (:|private_domains_for_user|
              ,(zs (lambda (doc)
                     (with-slots (type domain domain_name user role) doc
                       (when (and (eql type #.(docname 'potato.core:domain-user))
                                  (eql role "PRIVATE"))
                         (emit user (create :|domain| domain :|domain_name| domain_name :|role| role)))))))
            (:|domains_for_mail|
              ,(zs (lambda (doc)
                     (with-slots (type email_domains) doc
                       (when (eql type #.(docname 'potato.core:domain))
                         (dolist (v email_domains)
                           (emit v doc)))))))
            (:|invitations_for_email|
              ,(zs (lambda (doc)
                     (with-slots (type email) doc
                       (when (eql type #.(docname 'potato.core:domain-email-invitation))
                         (emit email doc))))))
            (:|email_invitations_for_domain|
              ,(zs (lambda (doc)
                     (with-slots (type domain) doc
                       (when (eql type #.(docname 'potato.core:domain-email-invitation))
                         (emit domain doc))))))
            (:|domain_list|
              ,(zs (lambda (doc)
                     (with-slots (type domain_type) doc
                       (when (eql type #.(docname 'potato.core:domain))
                         (emit domain_type doc))))))
            (:|public_domains|
              ,(zs (lambda (doc)
                     (with-slots (_id type join_public) doc
                       (when (and (eql type #.(docname 'potato.core:domain))
                                  join_public)
                         (emit _id doc))))))
            (:|domain_nickname|
              ,(zs (lambda (doc)
                     (with-slots (type domain) doc
                       (when (eql type #.(docname 'potato.core:domain-nickname))
                         (emit domain doc))))))))

  (mkview "group"
          `((:|groups_for_user|
              ,(zs (lambda (doc)
                     (with-slots (_id domain type name users group_type) doc
                       (when (eql type #.(docname 'potato.core:group))
                         (dolist (user users)
                           (with-slots (user_id role) user
                             (emit (list domain user_id role)
                                   (create :|group| _id
                                           :|group_name| name
                                           :|group_type| group_type
                                           :|role| role)))))))))
            (:|groups_for_user_nodomain|
              ,(zs (lambda (doc)
                     (with-slots (_id type name users group_type) doc
                       (when (eql type #.(docname 'potato.core:group))
                         (dolist (user users)
                           (with-slots (user_id role) user
                             (emit (list user_id role)
                                   (create :|group| _id
                                           :|group_name| name
                                           :|group_type| group_type
                                           :|role| role)))))))))
            (:|groups_and_users|
              ,(zs (lambda (doc)
                     (with-slots (_id type users) doc
                       (when (eql type #.(docname 'potato.core:group))
                         (dolist (user users)
                           (with-slots (user_id role) user
                             (emit (list _id user_id) role))))))))
            #+nil(:|available_groups_for_domain|
                   ,(zs (lambda (doc)
                          (with-slots (_id type authorised_domains) doc
                            (when (eql type "group")
                              (dolist (domain authorised_domains)
                                (emit domain _id)))))))
            (:|groups_in_domain|
              ,(zs (lambda (doc)
                     (with-slots (type domain group_type name) doc
                       (when (eql type #.(docname 'potato.core:group))
                         (emit domain (create :|group_type| group_type
                                              :|name| name)))))))))
  (mkview "file"
          `((:|files_for_channel|
              ,(zs (lambda (doc)
                     (with-slots (type channel name confirmed_p) doc
                       (when (and (eql type #.(docname 'potato.upload:file))
                                  (stringp confirmed_p))
                         (emit (list channel name) doc))))))
            (:|not_confirmed_files|
              ,(zs (lambda (doc)
                     (with-slots (type key created_date confirmed_p) doc
                       (when (and (eql type #.(docname 'potato.upload:file))
                                  (not (stringp confirmed_p)))
                         (emit (list created_date key) doc))))))
            (:|size_for_channel|
              ,(zs (lambda (doc)
                     (with-slots (type channel _id size confirmed_p) doc
                       (when (and (eql type #.(docname 'potato.upload:file))
                                  (stringp confirmed_p))
                         (emit (list channel _id) size)))))
              ,(zs (lambda (key values rereduce)
                     (sum values))))
            (:|size_for_user|
              ,(zs (lambda (doc)
                     (with-slots (type user channel _id size confirmed_p) doc
                       (when (and (eql type #.(docname 'potato.upload:file))
                                  (stringp confirmed_p))
                         (emit (list user channel _id) size)))))
              ,(zs (lambda (key values rereduce)
                     (sum values))))))

  (mkview "gcm"
          `((:|gcm_for_user|
              ,(zs (lambda (doc)
                     (with-slots (type user gcm_token recipient_type) doc
                       (when (eql type #.(docname 'potato.gcm:gcm-registration))
                         (emit user (list gcm_token (or recipient_type "GCM"))))))))
            (:|unread_channel|
              ,(zs (lambda (doc)
                     (with-slots (type user gcm_token unread recipient_type) doc
                       (when (eql type #.(docname 'potato.gcm:gcm-registration))
                         (dolist (cid unread)
                           (emit cid (list user gcm_token (or recipient_type "GCM")))))))))))

  (delete-document "_design/index_filter" :if-missing :ignore)
  (create-document `((:|filters| . ((:|index_updates| . ,(zs (lambda (doc req)
                                                               (with-slots (type updated) doc
                                                                 (or (eql type #.(docname 'potato.core:user))
                                                                     (eql type "memberdomain"))))))
                                    (:|created_date| . ,(potato.core:format-timestamp nil (local-time:now))))))
                   :id "_design/index_filter"))

(defun init-user-notification-views ()
  (potato.common:with-user-notification-db
    (mkview "user"
            `((:|notifications_for_user|
                ,(zs (lambda (doc)
                       (with-slots (type user created_date) doc
                         (when (eql type #.(docname 'potato.user-notification:user-notification))
                           (emit (list user created_date) doc))))))
              (:|notifications_for_user_unread|
                ,(zs (lambda (doc)
                       (with-slots (type user created_date read) doc
                         (when (and (eql type #.(docname 'potato.user-notification:user-notification))
                                    (not read))
                           (emit (list user created_date) doc))))))
              (:|notifications_for_user_channel|
                ,(zs (lambda (doc)
                       (with-slots (type user channel created_date read _id) doc
                         (when (and (eql type #.(docname 'potato.user-notification:user-notification))
                                    (not read))
                           (emit (list user channel created_date) (list _id)))))))
              (:|notifications_with_timestamps|
                 ,(zs (lambda (doc)
                        (with-slots (type read ping_timestamp user channel) doc
                          (when (and (eql type #.(docname 'potato.user-notification:user-notification))
                                     (not (null ping_timestamp))
                                     (not read))
                            (emit ping_timestamp (create :|user| user
                                                         :|channel| channel)))))))
              (:|notifications_updated|
                ,(zs (lambda (doc)
                       (with-slots (type ping_timestamp user read) doc
                         (when (and (eql type #.(docname 'potato.user-notification:user-notification))
                                    (not read)
                                    (not (null ping_timestamp)))
                           (emit ping_timestamp user)))))))
            `((:|mark_as_read|
                ,(zs (lambda (doc req)
                       (let ((uid (@ req query user)))
                         (with-slots (user read) doc
                           (cond ((not (eql user uid))
                                  (list nil "\"no-match\""))
                                 (read
                                  (list nil "\"already-marked-as-read\""))
                                 (t
                                  (setf read true)
                                  (list doc "\"ok\""))))))))))))

(defun init-messages-views ()
  (potato.common:with-messages-db
    (mkview "channel"
            `((:|created_date|
                ,(zs (lambda (doc)
                       (with-slots (_id type channel created_date) doc
                         (when (and (eql type "message"))
                           (emit (list channel _id) doc)))))))

            ;; Update functions
            `((:|update_message_text|
                ,(zs (lambda (doc req)
                       (labels ((copy-message ()
                                  (create :|updated_date| (or (getprop doc "updated_date")
                                                              (getprop doc "created_date"))
                                          :|deleted| (getprop doc "deleted")
                                          :|extra_html| (getprop doc "extra_html")
                                          :|text| (getprop doc "text")
                                          :|image| (getprop doc "image"))))
                         (let ((date (@ req query date))
                               (image (@ req query update_image))
                               (image-width (parse-int (@ req query update_image_width)))
                               (image-height (parse-int (@ req query update_image_height)))
                               (image-mime-type (@ req query update_image_mime_type))
                               (old-updated (getprop doc "update"))
                               (copy (copy-message)))
                           (if old-updated
                               (funcall (@ old-updated push) copy)
                               (setf (getprop doc "update") (list copy)))
                           (macrolet ((update-if-changed (value field &optional transformer-fn)
                                        `(let ((v (@ req query ,value)))
                                           (when v
                                             (setf (getprop doc ,field)
                                                   ,(if transformer-fn
                                                        `(funcall ,transformer-fn v)
                                                        'v))))))
                             (setf (getprop doc "updated_date") date)
                             (update-if-changed update_message_text "text")
                             (update-if-changed update_extra_html "extra_html")
                             (update-if-changed update_deleted_p "deleted" (lambda (p) (if (eql p "1") true false)))
                             (when (and image (not (eql image "")))
                               (setf (getprop doc "image")
                                     (create :|file| image
                                             :|width| image-width
                                             :|height| image-height)))
                             (list doc (funcall (@ *JSON* stringify)
                                                (create "result" "ok"
                                                        "updates" (length (getprop doc "update")))))))))))
              (:|update_star_user|
                ,(zs (lambda (doc req)
                       (let ((uid (@ req query user_id))
                             (add (eql (@ req query add) "1")))
                         (let* ((star-users (let ((u (getprop doc "star_users")))
                                              (if (eql u undefined)
                                                  (let ((res (list)))
                                                    (setf (getprop doc "star_users") res)
                                                    res)
                                                  u)))
                                (position (funcall (@ star-users index-of) uid))
                                (modified nil))
                           (cond ((and (>= position 0) (not add))
                                  (funcall (@ star-users splice) position 1)
                                  (setq modified t))
                                 ((and (= position -1) add)
                                  (funcall (@ star-users push) uid)
                                  (setq modified t)))
                           (list (if modified doc nil)
                                 (funcall (@ *JSON* stringify) (if position true false))))))))
              (:|update_hidden_user|
                ,(zs (lambda (doc req)
                       (let ((uid (@ req query user_id))
                             (add (eql (@ req query add) "1")))
                         (let* ((hidden-users (let ((u (getprop doc "hidden")))
                                              (if (eql u undefined)
                                                  (let ((res (list)))
                                                    (setf (getprop doc "hidden") res)
                                                    res)
                                                  u)))
                                (position (funcall (@ hidden-users index-of) uid))
                                (modified nil))
                           (cond ((and (>= position 0) (not add))
                                  (funcall (@ hidden-users splice) position 1)
                                  (setq modified t))
                                 ((and (= position -1) add)
                                  (funcall (@ hidden-users push) uid)
                                  (setq modified t)))
                           (list (if modified doc nil)
                                 (funcall (@ *JSON* stringify) (if position true false))))))))))

    (delete-document "_design/messagefilters" :if-missing :ignore)
    (create-document `((:|filters| . ((:|message_index_updates| . ,(zs (lambda (doc req)
                                                                         (with-slots (type updated) doc
                                                                           (and (eql type "message")
                                                                                (not updated))))))
                                      (:|created_date| . ,(potato.core:format-timestamp nil (local-time:now))))))
                     :id "_design/messagefilters")))

(defun init-views ()
  (init-potato-views)
  (init-user-notification-views)
  (init-messages-views))

(defun init-views-if-needed ()
  (let ((result (clouchdb:get-document "_design/user" :if-missing :ignore)))
    (unless result
      (init-views))))
