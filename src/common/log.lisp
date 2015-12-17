(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(defparameter *out* *standard-output*)
(defvar *log-location* nil)

(potato.common.application:define-component logging
  (:start
   #-abcl
   (when *log-location*
     (log:config :daily *log-location*))))
