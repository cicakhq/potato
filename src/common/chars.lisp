(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(defun truncate-string (string n &key append-dots)
  "Truncates STRING after a maximum of N characters.
If the Lisp implementation has full Unicode support, the truncation
will happen at a grapheme cluster boundary."
  (if (<= (length string) n)
      string
      ;; ELSE: String needs to be truncated
      (let* ((n (if append-dots (1- n) n))
             (truncated (progn
                          #+ (and sbcl sb-unicode)
                          (with-output-to-string (s)
                            (loop
                              for v in (sb-unicode:graphemes string)
                              for total = (length v) then (+ total (length v))
                              while (<= total n)
                              do (write-string v s)))
                          #-(and sbcl sb-unicode)
                          (subseq string 0 n))))
        (if append-dots
            (concatenate 'string truncated "…")
            truncated))))
