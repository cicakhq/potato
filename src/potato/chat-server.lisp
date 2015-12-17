(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defparameter *js-dirs* '("src/assets/vendor/bower/fineuploader-dist/dist"
                          "src/assets/vendor/bower/jquery/dist"
                          "src/assets/vendor/bower/expandingtextareas"
                          "src/assets/vendor/bower/blueimp-md5/js"
                          "src/assets/javascripts"
                          "public/assets/vendor"
                          "public/assets/js"
                          "web-app/resources/public/js"))

(defun extension-to-content-type (file)
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "\\.([^.]+)$" file)
    (if (null match)
        "binary/octet-stream"
        (let ((extension (aref strings 0)))
          (string-case:string-case (extension)
            ("html" "text/html")
            ("txt"  "text/plain")
            ("woff" "application/font-woff")
            ("jpg"  "image/jpeg")
            ("jpeg" "image/jpeg")
            ("gif"  "image/gif")
            ("js"   "application/javascript")
            ("json" "application/json")
            ("map"  "application/json")
            (t (error "No mime-type mapping for extension: ~a" extension)))))))

(defun %handle-js-file (file)
  (loop
     for dir-name in *js-dirs*
     for pathname = (merge-pathnames (pathname (concatenate 'string dir-name "/" file))
                                     (asdf:component-pathname (asdf:find-system :potato)))
     for full = (probe-file pathname)
     when full
     do (progn
          (setf (hunchentoot:content-type*) (extension-to-content-type file))
          (let ((out (hunchentoot:send-headers)))
            (with-open-file (in-stream full :element-type '(unsigned-byte 8))
              (uiop/stream:copy-stream-to-stream in-stream out :element-type '(unsigned-byte 8))
              (finish-output out)))
          (return nil))
     finally (progn
               (log:debug "Couldn't find file ~s, returning not-found" file)
               (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
               (format nil "Can't find file ~s" file))))

(lofn:define-handler-fn (js-screen "/assets/js/(.+)" t (file))
  (%handle-js-file file))

(lofn:define-handler-fn (react-js-screen "/js/(.+)" t (file))
  (if *debug*
      (hunchentoot:redirect (hunchentoot:request-uri*) :port 10555)
      (%handle-js-file file)))

(lofn:define-json-handler-fn (build-id-screen "/build_id" data nil ())
  (declare (ignore data))
  (st-json:jso "version" potato:*build-id*))

(lofn:define-handler-fn (eventsource-test-screen "/estest" nil ())
  (setf (hunchentoot:header-out :cache-control) "no-cache")
  (setf (hunchentoot:content-type*) "text/event-stream")
  (let ((out (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
    (loop
       repeat 5
       for i from 0
       do (progn
            (format out "data:")
            (st-json:write-json (st-json:jso "text" "Test message" "index" i) out)
            (format out "~c~c" #\Newline #\Newline)
            (finish-output out)
            (sleep 1)))))

(defun start-web ()
  (setq hunchentoot:*rewrite-for-session-urls* nil)
  (setq lofn:*simple-files-base-dir*
        #+nil(merge-pathnames #p"src/assets/" (asdf:component-pathname (asdf:find-system :potato)))
        (asdf:component-pathname (asdf:find-system :potato)))
  (setq lofn:*template-files-base-dir*
        (merge-pathnames #p"src/template/" (asdf:component-pathname (asdf:find-system :potato))))
  (lofn:start-server :address *listen-address*
                     :port *listen-port*
                     :dirs '(("public/assets/" "/assets/"))
                     :acceptor-name 'potato-acceptor))

(potato.common.application:define-component main-web-server
  (:dependencies potato.common::generic potato.db::db potato.upload::upload)
  (:start
   (start-state-listener-thread)
   (start-web)
   (potato.message-update:start-message-update-thread)
   (log:info "Server started")))
