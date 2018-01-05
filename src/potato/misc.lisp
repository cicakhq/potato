(in-package :potato)

(declaim #.potato.common::*compile-decl*)

(defvar *external-address* nil)
(defvar *external-listen-address* nil)
(defvar *external-listen-port* 8080)
(defvar *listen-address* nil)
(defvar *listen-port* 8080)
(defvar *websocket-listen-port* 8081)
(defvar *external-websocket-listen-address* "ws://localhost:8081/ws")
(defvar *allowed-origin* "*")

(defvar *email-type* nil
  "Email provider type. Valid values are NIL (off), :SMTP or :MAILGUN")

(defvar *smtp-server-host* nil)
(defvar *smtp-server-port* 25)
(defvar *smtp-username* nil)
(defvar *smtp-password* nil)
(defvar *smtp-ssl* nil)

(defvar *mailgun-key* nil)
(defvar *mailgun-user-domain* nil)

(defvar *allow-passwordless-login* nil)
(defvar *allow-password-recovery* t)

(defvar *user-registration-validation-function* nil)
(defvar *allow-registration-without-domain* t
  "If false, do not allow registration of a user which does not have access to at least one domain.")
