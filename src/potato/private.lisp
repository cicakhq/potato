(in-package :potato.private)

(declaim #.potato.common::*compile-decl*)

(defparameter *privgrp-group-prefix* "privgrp-")

(defun find-private-channel-for-users (domain u1 u2)
  (let ((domain (potato.core:ensure-domain domain)))
    (unless (and (potato.core:user-is-in-domain-p domain u1)
                 (potato.core:user-is-in-domain-p domain u2))
      (error "Users ~a and ~a are not both in domain ~s" u1 u2 domain))
    (let ((u1-id (potato.core:ensure-user-id u1))
          (u2-id (potato.core:ensure-user-id u2)))
      ;; A user is not allowed to start a chat with itself
      (when (string= u1-id u2-id)
        (potato.core:raise-web-parameter-error "Not allowed to create a private channel with itself"))
      ;; Make sure that the id of the group and channel is not dependent
      ;; on the order of the users.
      (when (string> u1-id u2-id)
        (rotatef u1-id u2-id))
      ;; Find the private group
      (let* ((group-id (format nil "~a~a-~a-~a"
                               *privgrp-group-prefix*
                               (encode-name (potato.core:domain/id domain))
                               (encode-name u1-id)
                               (encode-name u2-id)))
             (group (potato.db:load-instance 'potato.core:group group-id :error-if-not-found nil)))
        (unless group
          ;; Group did not exist, attempt to create it
          (let ((gr (make-instance 'potato.core:group
                                   :type :private
                                   :name (format nil "Private chat group for users ~a, ~a" u1-id u2-id)
                                   :domain (potato.core:domain/id domain)
                                   :users `(((:|user_id| . ,u1-id)
                                             (:|role| . "PRIVATE"))
                                            ((:|user_id| . ,u2-id)
                                             (:|role| . "PRIVATE")))
                                   :couchdb-id group-id)))
            ;; TODO: The save may actually fail here if another attempt at creating
            ;; the group was made at exactly the same time. We'll disregard
            ;; of that for now.
            (potato.db:save-instance gr)
            ;; Group has been created, now create the default channel
            (let ((ch (potato.core:create-channel (format nil "Private channel for users ~a, ~a" u1-id u2-id)
                                                  gr (list u1-id u2-id))))
              (log:debug "Private channel created for users ~a,~a: ~s" u1-id u2-id ch))
            (setf group gr)))
        ;; TODO: Right now we will simply return the default private
        ;; channel (possibly just created). We might want to change this
        ;; to allow multiple private channels for a given user pair.
        (let ((channels (potato.core:find-channels-for-group group)))
          (when (null channels)
            (error "No channel found in private group: ~s" group))
          (when (> (length channels) 1)
            (log:error "More than one channel in private group: ~s, using the first channel" group))
          (car channels))))))

(defun find-private-channels-for-domain-and-user (domain user)
  (let* ((domain-id (potato.core:ensure-domain-id domain))
         (user-id (potato.core:ensure-user-id user))
         (result (potato.db:invoke-view-and-load-instances 'potato.core:channel-users "channel" "private_channel_list"
                                                           :key (list domain-id user-id))))
    result))

(defun find-chat-counterpart (channel-users self-user)
  "Return the user with whom SELF-USER is chatting. Raise an error if
there are any inconsistencies in the instance."
  (let ((users (potato.core:channel-users/users channel-users)))
    (unless (eq (potato.core:channel-users/group-type channel-users) :private)
      (error "~s does not represent a private chat" channel-users))
    (unless (= (length users) 2)
      (error "A private chat should only have two users: ~s" channel-users))
    (let ((self-user-keyword (intern (potato.core:ensure-user-id self-user) "KEYWORD"))
          (u1 (first users))
          (u2 (second users)))
      (cond ((eq (car u1) self-user-keyword)
             (symbol-name (car u2)))
            ((eq (car u2) self-user-keyword)
             (symbol-name (car u1)))
            (t
             (error "~s is not a member of channel ~s" self-user channel-users))))))
