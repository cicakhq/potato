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

; erlang is a super dependency of couchdb and rabbitmq
; openjdk8 pulls libXt and depends on a whole list of X-related packages
; + TODO: see https://www.digitalocean.com/community/tutorials/how-to-install-java-on-freebsd-10-1, modify `fstab'
(dolist (p '("sudo" "curl" "git" "bzip2" "zip" "unzip" "bash" "gnutls" "openssl" "ImageMagick-nox11"
             "autoconf" "libtool" "automake" ;; for libfixposix
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

(uiop:chdir "/tmp")

(unless (uiop:directory-exists-p "/usr/local/solr-5.4.0")
  (if (uiop:file-exists-p "/tmp/solr-5.4.0.tgz")
      (uiop:run-program (list "/sbin/md5" "-c" "f906356e01eebb08e856a7c64250ba53" "solr-5.4.0.tgz"))
      (progn
        (format t "Downloading SolR~%")
        (uiop:run-program (list "/usr/local/bin/curl" "-O" "http://www.us.apache.org/dist/lucene/solr/5.4.0/solr-5.4.0.tgz"))
        (uiop:run-program (list "/sbin/md5" "-c" "f906356e01eebb08e856a7c64250ba53" "solr-5.4.0.tgz"))))

  (format t "Extracting SolR~%")
  (uiop:run-program (list "/usr/bin/tar" "-zx" "-C" "/usr/local" "-f" "/tmp/solr-5.4.0.tgz")))

(format t "Installing libfixposix~%")
(uiop:chdir "/tmp")
(uiop:run-program (list "/usr/local/bin/curl" "-o" "libfixposix.zip" "https://codeload.github.com/sionescu/libfixposix/zip/master"))
(uiop:run-program (list "/usr/bin/unzip" "/tmp/libfixposix.zip"))
(uiop:chdir "/tmp/libfixposix-master")
(uiop:run-program (list "/usr/local/bin/autoreconf" "-i" "-f"))
(uiop:run-program (list "./configure"))
(uiop:run-program (list "make"))
(uiop:run-program (list "make" "install"))


