(in-package :potato.email)

(declaim #.potato.common::*compile-decl*)

(defvar *potato-sender-address* "Potato <noreply@the-potato.com>")

(defclass mail-descriptor ()
  ((to-name      :type (or null string)
                 :initarg :to-name
                 :initform nil
                 :reader mail-descriptor/to-name)
   (to-email     :type string
                 :initarg :to-email
                 :initform (error "~s is a required parameter" :to-email)
                 :reader mail-descriptor/to-email)
   (subject      :type string
                 :initarg :subject
                 :initform (error "~s is a required parameter" :subject)
                 :reader mail-descriptor/subject)
   (text-content :type string
                 :initarg :text-content
                 :initform nil
                 :reader mail-descriptor/text-content)
   (html-content :type (or null string)
                 :initarg :html-content
                 :initform nil
                 :reader mail-descriptor/html-content))
  (:documentation "Description of an email"))

(defmethod initialize-instance :after ((obj mail-descriptor) &key)
  (when (and (null (mail-descriptor/text-content obj))
             (null (mail-descriptor/html-content obj)))
    (error "text and html content can't both be unset")))

(defmethod print-object ((obj mail-descriptor) stream)
  (print-unreadable-safely (to-name to-email subject) obj stream
    (format stream "~a ~a: ~a" to-name to-email subject)))

(defun make-mail-for-user (user subject text-content html-content)
  (unless html-content
    (error "Only HTML content is supported at the moment"))
  (let ((user (potato.core:ensure-user user)))
    (make-instance 'mail-descriptor
                   :to-name (potato.core:user/description user)
                   :to-email (potato.core:user/primary-email user)
                   :subject subject
                   :text-content text-content
                   :html-content html-content)))

(defun send-email (mail)
  (check-type mail mail-descriptor)
  (log:trace "Sending email to: ~s, subject: ~s" (mail-descriptor/to-email mail) (mail-descriptor/subject mail))
  (with-pooled-rabbitmq-connection (conn)
    (cl-rabbit:basic-publish conn 1
                             :exchange *email-exchange-name*
                             :body (conspack:encode mail))))

(defun encode-destination (mail)
  (if (mail-descriptor/to-name mail)
      (format nil "~a <~a>"
              (cl-smtp:rfc2045-q-encode-string (mail-descriptor/to-name mail))
              (mail-descriptor/to-email mail))
      (format nil "<~a>" (mail-descriptor/to-email mail))))

(defun post-smtp-message (mail)
  (unless *smtp-server-host*
    (log:warn "No SMTP server configured, email to ~a will not be sent" (mail-descriptor/to-email mail))
    (return-from post-smtp-message))
  (log:trace "Sending email via SMTP to: ~a" (mail-descriptor/to-name mail))
  (let ((params (append (list *smtp-server-host*
                              *potato-sender-address*
                              (encode-destination mail)
                              (mail-descriptor/subject mail)
                              (or (mail-descriptor/text-content mail) "This is a HTML message")
                              :ssl *smtp-ssl*)
                        (if (mail-descriptor/html-content mail)
                            (list :html-message (mail-descriptor/html-content mail)))
                        (if *smtp-server-port*
                            (list :port *smtp-server-port*))
                        (if *smtp-username*
                            (list :authentication (list :login *smtp-username* *smtp-password*))))))
    (log:trace "Calling send-email with params: ~s" params)
    (handler-case
        (apply #'cl-smtp:send-email params)
      (cl-smtp:rcpt-failed (condition) (log:error "Unable to send email to ~s: ~a"
                                                  (mail-descriptor/to-email mail) condition)))))

(defun post-mailgun-message (mail)
  (unless (and *mailgun-key* *mailgun-user-domain*)
    (log:warn "No mailgun key configured, email will not be sent")
    (return-from post-mailgun-message))
  (log:trace "Sending email via mailgun to: ~a" (mail-descriptor/to-name mail))
  (mailgun:send-message *potato-sender-address* (list (encode-destination mail)) (mail-descriptor/subject mail)
                        :user-domain *mailgun-user-domain*
                        :api-key *mailgun-key*
                        :content (or (mail-descriptor/text-content mail) "This is a HTML message")
                        :html-content (mail-descriptor/html-content mail)))

(defun email-sender-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *email-queue-name*)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       for body = (cl-rabbit:message/body (cl-rabbit:envelope/message msg))
       for mail-descriptor = (potato.common:decode-conspack-with-interning body)
       do (progn
            (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag msg))
            (ecase *email-type*
              (:smtp (post-smtp-message mail-descriptor))
              (:mailgun (post-mailgun-message mail-descriptor)))))))

(defun start-email-sender-thread ()
  (if *email-type*
      (start-monitored-thread #'email-sender-loop "Email sender")
      (log:info "No email provider configured. Not starting email sender.")))
