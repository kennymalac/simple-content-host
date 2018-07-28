(defpackage :config
  (:use :common-lisp)
  (:export #:*hostname* #:*port* #:*tmp-location*))

(in-package :config)


(defparameter *hostname* "127.0.0.1")
(defparameter *port* 4242)
(defparameter *tmp-location* (merge-pathnames "tmp/"))
