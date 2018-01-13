(in-package :asdf-user)

(defsystem "simple-content-host"
  :description "An HTTP interface for TinyCDN"
  :version "0.0.1"
  :depends-on (#:cl-async #:cffi #:wookie)
  :components
  (
   (:module "program"
    :components ((:file "filebucket")
                (:file "slave" :depends-on ("filebucket"))))
  ))

