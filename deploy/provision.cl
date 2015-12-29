#!/usr/bin/env sbcl

(require :asdf)

(defconstant *freebsd-type* "FreeBSD")

(defun freebsd-update ()
  (format t "Checking for FreeBSD patches~%")
  (uiop:run-program (list "/usr/sbin/freebsd-update" "Fetch") :ignore-error-status t)
  (uiop:run-program (list "/usr/sbin/freebsd-update" "install") :ignore-error-status t))

(defun portsnap (an-action)
  (format t "Portsnap ~s~%" an-action)
  (uiop:run-program (list "/usr/sbin/portsnap" "--interactive" an-action) :output nil))

(defun cmd-package-install (a-package)
  (cond ((string= (software-type) *freebsd-type*)
         (format t "FreeBSD pkg installing ~s~%" a-package)
         (uiop:run-program `("/usr/sbin/pkg" "install" "-yqU" ,a-package)))
        (t (error "Platform ~s (~s) on ~s is not supported~%" (software-type) (software-version) (machine-type)))))

(defun cmd-package (a-command a-package)
  (cond ((string= a-command "install") (cmd-package-install a-package))))

(defun pkg-info (a-package)
  (uiop:run-program (list "/usr/sbin/pkg" "info" a-package)))

;-- MAIN

(format t "Hello World!~%")
(freebsd-update)

; help2man is a dependency of couchdb
; openssl is a dependency of erlang
; erlang is a super dependency of couchdb and rabbitmq
; gmake is a dependency of couchdb
; openjdk8 pulls libXt and depends on a whole list of X-related packages
; + TODO: see https://www.digitalocean.com/community/tutorials/how-to-install-java-on-freebsd-10-1, modify `fstab'
(dolist (p '("curl" "git" "bzip2" "zip" "unzip" "bash" "gnutls" "openssl" "help2man" "gmake"
             "erlang" "couchdb" "rabbitmq" "openjdk8" "memcached" "rabbitmq-c-devel" "leiningen"))
  (cmd-package "install" p))

(portsnap "fetch")
(handler-case
    (portsnap "update")
  ;; update failed, maybe we should try "extract" for the initial portsnap
  (uiop:subprocess-error () (portsnap "extract")))

(handler-case
    (pkg-info "nginx-devel")
  ;; "info" failed, which probably means we do not have nginx-devel installed yet
  ;; TODO: manage updates?
  (uiop:subprocess-error ()
    (format t "Installing nginx~%")
    (uiop:chdir "/usr/ports/www/nginx-devel")
    (uiop:run-program (list "/usr/bin/make" "-DWITH=\"HTTPV2\"" "install" "clean" "BATCH=yes"))))

