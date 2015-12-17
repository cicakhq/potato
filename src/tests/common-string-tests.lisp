(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(define-test ensure-printable-string-test ()
  (let ((foo "abc"))
    (assert-eq foo (ensure-printable-string foo)))
  (loop
     for i from 0 to #x1f
     for ch = (code-char i)
     if (not (or (eql ch #\Newline)
                 (eql ch #\Tab)))
     do (let ((foo (format nil "abc~cfoo" ch)))
          (assert-equal "abcfoo" (ensure-printable-string foo)))
     else
     do(let ((foo (format nil "abc~cfoo" ch)))
         (assert-eq foo (ensure-printable-string foo)))))
