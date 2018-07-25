(defpackage :filebucket
  (:use :common-lisp :cffi :alexandria :cffi-utils)
  (:export #:with-upload-session #:content-file #:file-bucket #:file-upload-info #:upload-session #:upload-content #:serialize))

(in-package :filebucket)

(define-foreign-library libtinycdn
  (t (:default "/home/ken/Programming/cpp/tinycdn/build/libtinyCDN")))

(use-foreign-library libtinycdn)

(defclass upload-session ()
  ((id :reader id :initarg :id)
   (ptr :accessor ptr :initarg :ptr)
   (bucket :accessor bucket :initarg :bucket)
   (content-file :accessor content-file :initarg :content-file)))

(defmacro with-upload-session (session &body body)
  `(prog1
       ,@body
     (delete-ptr ,session)))

(defcstruct upload-session "FileUploadingSession")
(defcmethod upload-session "new" new :pointer)
(defcmethod upload-session "delete" delete-ptr :void t ()
  (setf (ptr self) nil))

(defcmethod upload-session "finishFileUpload" finish-file-upload :void t ((char-array :pointer)))

(defcmethod upload-session "getBucket" get-bucket :int t ()
  (setf (bucket self) (make-instance 'file-bucket :id it)))

(defmethod initialize-instance :around ((self upload-session) &key id)
  (call-next-method self :id id
                    ;;  cffi-call :int id
                    :ptr (new self)))

(defclass file-upload-info ()
  ((ptr :accessor ptr :initarg nil)))

(defcstruct file-upload-info "FileUploadInfo")
(defcmethod file-upload-info "new" new :pointer nil
    ((temporary-location :string) (content-type :string) (tags :pointer) (wantsOwned :int)))


(defclass content-file ()
  ((id :initarg :id)
   (uri :accessor uri :initform nil)
   (bucket-id :reader bucket-id :initarg :bucket-id)
;;   (is-dirty :accessor is-dirty :initform t)
   (has-uploaded :accessor has-uploaded :initform nil)
   (file-meta :accessor file-meta :initarg :file-meta :initform (make-hash-table))
   ;;  (chunks :initform '())
   (tmp-file-location :accessor tmp-file-location :initform nil)))


;; (defclass video-file (content-file))
;; (defclass audio-file (content-file))


;; (defmethod get-content-file-name ((ptr foreign-pointer))
;;   (foreign-string-to-lisp (foreign-funcall "getUniqueFileId" :pointer ptr)))

(defclass file-bucket ()
  ((id :accessor id :initarg :id)
   ;(location :initarg :location)
   ;; (nameserver )
   ;; you can enabled replication to geo-enabled nodes
   ;; (location )
   ))

;; TODO with-filebucket to discard filebucket if it isn't "hot"

;; (defmethod download ((ownedSelf bucket)))

;; (defmethod initialize-instance :around ((self file-bucket))
;;   (call-next-method self :id (foreign-string-to-lisp (foreign-funcall "getFileBucketId" :pointer ptr content-type file-type))
;;                     :location (foreign-string-to-lisp (foreign-funcall "getFileBucketLocation" ptr))))

(defmethod serialize ((self content-file))
  (list :uri (uri self)))


(defmethod upload-content :before ((self file-bucket) (session upload-session) file-info process)
  ;; NOTE two concurrent uploads of same filename would fail
  (let ((filename
         (gethash 'filename (file-meta (content-file session)))))
    ;; TODO assure permissions to upload to /tmp/ before failing...
    ;; :mime-type (getf field-headers :content-type)
    (call-next-method self session (list :filename filename
                            :tmp-file filename))))

(defmethod upload-content ((self file-bucket) (session upload-session) file-info process)
  ;; TODO don't destructure filename again!
  (let ((filename (getf file-info :filename)))
    (with-open-file (handle filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      ;; give a tmpfile handler to write chunking to
      (apply process handle)
      (content-file session))))

(defmethod upload-content :after ((self file-bucket) (session upload-session) file-info process)
  (with-foreign-object (cffi-result :pointer 2)
    (let* ((content (content-file session))
           (info (make-instance 'file-upload-info))
           (result (finish-file-upload session (tmp-file-location content) cffi-result info)))
      (setf (id content) (foreign-string-to-lisp (cdr result)))
      (setf (has-uploaded content) t)
      (setf (uri content) (concatenate 'string config:*hostname* (string config:*port*) (id content)))
      content)
  ;; generate-manifest
  ;; raw-to-dirty-chunks
  ;; dirty-to-clean-chunks
  ;; assign-uri
  ;; (uri content)
  ))

;; TODO optionally Distributed task - comslave market

(defmethod raw-to-dirty-chunks ((content content-file) (session upload-session)))
;; (defmethod dirty-to-clean-chunks ((chunks list))
;;   (return cleaned))
(defmethod assign-uri (content content-file))
;; (defmethod get-content-location (content content-file)
;;   (slot-value content uri))
