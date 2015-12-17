(in-package :potato.usocket)

(declaim #.potato.common::*compile-decl*)

(defun wait-until-activity (socket callback)
  (loop
     for sockets = (multiple-value-bind (sockets remaining)
                       (usocket:wait-for-input (list socket) :ready-only t)
                     (declare (ignore remaining))
                     sockets)
     if sockets
     return (funcall callback)))

(defun monitor-connection (socket callback)
  (bordeaux-threads:make-thread (lambda () (wait-until-activity socket callback))))
