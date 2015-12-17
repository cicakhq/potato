(in-package :potato.common)

(declaim #.*compile-decl*)

(defun make-fsmap ()
  (dhs-sequences:make-cas-wrapper (fset:empty-map)))

(defun fsmap-value (map key)
  (fset:lookup (dhs-sequences:cas-wrapper/value map) key))

(defun fsmap-set (map key value &key no-replace)
  (loop
     for old-map = (dhs-sequences:cas-wrapper/value map)
     do (progn
          (when no-replace
            (multiple-value-bind (old-value set-p)
                (fset:lookup old-map key)
              (when set-p
                (return old-value))))
          (let* ((new-map (fset:with old-map key value))
                 (result (dhs-sequences:cas map old-map new-map)))
            (when (eq old-map result)
              (return value))))))

(defun (setf fsmap-value) (value map key)
  (fsmap-set map key value))
