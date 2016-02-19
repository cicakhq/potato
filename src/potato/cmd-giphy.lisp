(defpackage :potato.slashcommand.giphy
  (:use :cl :potato :potato.common))

(in-package :potato.slashcommand.giphy)

(defun process-gif-command (args)
  (log:info "Got gif command: ~s" args))

(defun process-command-loop ()
  (potato.slashcommand:command-processor-loop args
   ("gif" (process-gif-command args))))

(potato.common.application:define-component slashcommand-giphy
  (:dependencies potato.common::rabbitmq)
  (:start
   (start-monitored-thread #'process-command-loop "Giphy command processor")))
