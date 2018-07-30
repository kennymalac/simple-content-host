(in-package :asdf-user)

(defsystem "simple-content-host"
  :description "An HTTP interface for TinyCDN"
  :defsystem-depends-on ("cffi-grovel")
  :version "0.0.1"
  :depends-on (#:cl-async #:cffi #:wookie #:alexandria #:yason #:babel #:vom)
  :components
  ((:module "vendor"
            :components ((:file "base32-crockford")))
   (:module "program"
            :depends-on ("vendor")
            :components (
                         (:file "config")
                         (:file "cffi-utils" :depends-on ("config"))
                         (:file "filebucket" :depends-on ("cffi-utils"))
                         (:file "slave" :depends-on ("config" "filebucket"))))))

