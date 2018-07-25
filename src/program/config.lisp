(defpackage :config
  (:use :common-lisp)
  (:export #:*hostname* #:*port*))

(in-package :config)


(defparameter *hostname* "127.0.0.1")
(defparameter *port* 4242)
