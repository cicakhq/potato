#!/usr/bin/env sbcl

(require :asdf)

(defconstant *freebsd-type* "FreeBSD")

(defun freebsd-update ()
  (format t "Checking for FreeBSD patches~%")
  (uiop:run-program (list "/usr/sbin/freebsd-update" "Fetch") :ignore-error-status t)
  (uiop:run-program (list "/usr/sbin/freebsd-update" "install") :ignore-error-status t))

(defun portsnap-fetch-update ()
  (format t "Portsnap refresh of ports~%")
  (uiop:run-program (list "/usr/sbin/portsnap" "--interactive" "fetch" "update") :output nil))

(defun cmd-package-install (a-package)
  (cond ((string= (software-type) *freebsd-type*)
         (format t "FreeBSD pkg installing ~s~%" a-package)
         (uiop:run-program `("/usr/sbin/pkg" "install" "-yqU" ,a-package)))
        (t (error "Platform ~s (~s) on ~s is not supported~%" (software-type) (software-version) (machine-type)))))

(defun cmd-package (a-command a-package)
  (cond ((string= a-command "install") (cmd-package-install a-package))))

;-- MAIN

(format t "Hello World!~%")
(freebsd-update)
(dolist (p '("curl" "git" "bzip2" "zip" "unzip" "bash" "gnutls"))
  (cmd-package "install" p))

(portsnap-fetch-update)
(format t "Installing nginx~%")
(uiop:chdir "/usr/ports/www/nginx-devel")
(uiop:run-program (list "/usr/bin/make" "-DWITH=\"HTTPV2\"" "install" "clean" "BATCH=yes"))

