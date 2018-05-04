(in-package :potato.image-convert)

(declaim #.potato.common::*compile-decl*)

(defvar *imagemagick-convert-program* "/usr/bin/convert")

(defparameter *resized-width* 700)
(defparameter *resized-height* 700)

(defgeneric process-result (task result)
  (:documentation "Called after the image has been converted"))

(defun find-image-convert-result (s)
  (let ((parts (split-sequence:split-sequence #\Newline s)))
    (loop
      for line in parts
      for result = (multiple-value-bind (match strings)
                       (cl-ppcre:scan-to-strings "^[a-zA-Z0-9._/-]+=>[a-zA-Z0-9._/-]+ [A-Z]+ [0-9]+x[0-9]+=>([0-9]+)x([0-9]+)"
                                                 line)
                     (if match
                         (list (parse-integer (aref strings 0)) (parse-integer (aref strings 1)))
                         nil))
      when result
        return result)))

(defvar *err* nil)
(defun resize-and-convert-image (infile outfile width height)
  "Resize INFILE using the given WIDTH and HEIGHT and write the output
to OUTFILE. Returns a list of two values: the resulting width and
height, or NIL if the resulting size could not be determined."
  (let ((args (list *imagemagick-convert-program*
                    "-verbose"
                    "-auto-orient"
                    (namestring infile)
                    "-resize"
                    (format nil "~ax~a" width height)
                    (namestring outfile))))
    (multiple-value-bind (out-string error-string)
        (uiop/run-program:run-program args :output :string :error-output :string)
      (log:info "args: ~s" args)
      (setq *err* error-string)
      ;; The output from convert -verbose are two lines on stderr of the following form:
      ;;   f.png PNG 1186x1427 1186x1427+0+0 8-bit DirectClass 70.6KB 0.020u 0:00.019
      ;;   f.png=>foo.png PNG 1186x1427=>166x200 166x200+0+0 8-bit DirectClass 4.1KB 0.110u 0:00.019
      ;;
      ;; There seems to be two versions of this program. The first version sends all output to stderr
      ;; while the second sends the first line to stderr and the other to stdout. We need to handle
      ;; both of these cases.
      (or (find-image-convert-result out-string)
          (find-image-convert-result error-string)
          (progn
            (log:warn "Unable to parse result of call image resize: ~s" error-string)
            nil)))))

(defun process-image (input width height callback)
  (unwind-protect
       (with-temp-file (name ".png")
         (when (resize-and-convert-image (namestring input) name width height)
           (funcall callback name)))
    ;; Unwind form: delete the input file
    (delete-file input)))

(defmethod process-result (user-id file)
  (let ((content (flexi-streams:with-output-to-sequence (s :element-type '(unsigned-byte 8))
                   (with-open-file (in file :element-type '(unsigned-byte 8))
                     (uiop/stream:copy-stream-to-stream in s :element-type '(unsigned-byte 8))))))
    (log:debug "Save image: ~s" user-id)
    (potato.user-image:user-save-image user-id content)))

(defun convert-user-image (file user-id)
  (process-image file 64 64 (lambda (f) (process-result user-id f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chat image processor main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *file-types* '(("image/jpg" "jpg")
                             ("image/jpeg" "jpg")
                             ("image/png" "png")
                             ("image/gif" "gif")))

(defun handle-chat-image-request (conn req)
  (destructuring-bind (file-id message-id) req
    (log:debug "Incoming chat image conversion request. file-id=~s, message-id=~s" file-id message-id)
    (let* ((file (potato.db:load-instance 'potato.upload:file file-id))
           (channel (potato.db:load-instance 'potato.core:channel (potato.upload:file/channel file))))
      (alexandria:when-let ((type-data (assoc (potato.upload:file/mime-type file) *file-types* :test #'equal)))
        (log:trace "Converting type=~s" type-data)
        (destructuring-bind (ext) (cdr type-data)
          (let ((ext-with-period (concatenate 'string "." ext)))
            (with-temp-file (infile ext-with-period)
              (log:trace "Downloading file ~s to ~s" file-id infile)
              (potato.upload:download-file-by-filesource file-id infile)
              (with-temp-file (outfile ext-with-period)
                (alexandria:when-let ((size (resize-and-convert-image infile outfile *resized-width* *resized-height*)))
                  (let* ((pictures-key (format nil "~a~a" (make-random-name 40) ext-with-period))
                         (uploaded-file (potato.upload:upload-file-by-name outfile pictures-key
                                                                           (potato.upload:file/mime-type file)
                                                                           message-id
                                                                           "images")))
                    (log:trace "Converted image: ~s" uploaded-file)
                    (cl-rabbit:basic-publish conn 1
                                             :exchange *chat-image-converter-response-exchange-name*
                                             :routing-key (potato.core:channel/id channel)
                                             :body (lisp-to-binary `(:update
                                                                     (,message-id
                                                                      :image-key ,(potato.upload:file/id uploaded-file)
                                                                      :image-size ,size))))))))))))))

(defun chat-image-processor-main-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *chat-image-converter-acceptor-queue-name*)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       ;; Ack before processing. We don't really want to reprocess the message if the conversion failed.
       do (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag msg))
       do (let ((decoded-command (binary-to-lisp (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))))
            (handle-chat-image-request conn decoded-command)))))

(defun start-chat-image-processor-thread ()
  (start-monitored-thread #'chat-image-processor-main-loop "Chat image processor thread"))
