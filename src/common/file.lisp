(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(defvar *random-name-chars*
  (coerce (append (loop
                     for i from (char-code #\a) to (char-code #\z)
                     append (list (code-char i) (char-upcase (code-char i))))
                  (loop
                     for i from (char-code #\0) to (char-code #\9)
                     collect (code-char i)))
          'vector))

(defun make-random-name (num-chars)
  (with-output-to-string (s)
    (loop
       with length = (length *random-name-chars*)
       repeat num-chars
       do (write-char (aref *random-name-chars* (secure-random:number length)) s))))

(defun call-with-temp-file-created (extension fn)
  (let ((file-name (loop
                      with prefix = (make-random-name 20)
                      with tmpdir = (uiop/stream:temporary-directory)
                      repeat 10
                      for i from 0
                      do (let* ((name (format nil "~a_~a~a" prefix i extension))
                                (path (merge-pathnames name tmpdir))
                                (stream (open path :direction :output :if-does-not-exist :create :if-exists nil)))
                           (when stream
                             (close stream)
                             (return path))))))
    (unless file-name
      (error "Unable to create random file"))
    (unwind-protect
         (funcall fn file-name)
      (delete-file file-name))))

(defmacro with-temp-file ((name extension) &body body)
  (let ((name-sym (gensym))
        (extension-sym (gensym)))
    `(let ((,extension-sym ,extension))
       (call-with-temp-file-created ,extension-sym (lambda (,name-sym)
                                                     (let ((,name ,name-sym))
                                                       ,@body))))))
