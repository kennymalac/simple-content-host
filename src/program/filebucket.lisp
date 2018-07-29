(defpackage :filebucket
  (:use :common-lisp :cffi :alexandria :cffi-utils)
  (:export #:with-upload-session #:content-file #:file-bucket #:get-bucket #:file-upload-info #:upload-session #:in-progress #:upload-content #:serialize #:delete-ptr #:ptr #:info))

(in-package :filebucket)

(define-foreign-library libtinycdn
  (t (:default "/home/ken/Programming/cpp/tinycdn/build/libtinyCDN")))

(use-foreign-library libtinycdn)

(defgeneric ptr (self))

(defclass upload-session ()
  ((id :reader id :initarg :id)
   (ptr :accessor ptr :initarg :ptr)
   (bucket :accessor bucket :initarg :bucket :initform nil)
   (in-progress :accessor in-progress :initform nil)
   (content-file :accessor content-file :initarg :content-file)))

(defmacro with-upload-session (session &body body)
  `(unwind-protect
        (progn ,@body)
     (delete-ptr ,session)))

(defcclass upload-session "FileUploadingSession")
(defcmethod upload-session "new" new :pointer)
(defcmethod upload-session "delete" delete-ptr :void ptr ()
  (setf (ptr self) nil))

(defcmethod upload-session "finishFileUpload" finish-file-upload :void ptr ((char-array :pointer) (upload-info :pointer)))

(defcmethod upload-session "getBucket" get-bucket :int ptr ((upload-info :pointer))
  (setf (bucket self) (make-instance 'file-bucket :id it)))

(defmethod initialize-instance :around ((self upload-session) &key id content-file)
  (call-next-method self :id id
                    :content-file content-file
                    ;;  cffi-call :int id
                    :ptr (new self)))

(defclass file-upload-info ()
  ((ptr :accessor ptr :initarg :ptr)))

(defcclass file-upload-info "FileUploadInfo")
(defcmethod file-upload-info "new" new :pointer nil
    ((temporary-location :string) (content-type :string) (file-type :string) (tags :pointer) (wants-owned :int)))

(defmethod initialize-instance :after ((self file-upload-info) &key tmp-location content-type file-type tags wants-owned)
  (format t "~{~a~^, ~}~%" (list tmp-location content-type file-type tags wants-owned))
  (with-foreign-array (c-tags tags (dynamic-c-array-type tags))
    (setf (ptr self) (new self tmp-location content-type file-type c-tags wants-owned))))

(defclass content-file ()
  ((id :accessor id :initarg :id :initform nil)
   (uri :accessor uri :initform nil)
   (bucket-id :accessor bucket-id :initform nil :initarg :bucket-id)
;;   (is-dirty :accessor is-dirty :initform t)
   (has-uploaded :accessor has-uploaded :initform nil)
   (file-meta :accessor file-meta :initarg :file-meta :initform (make-hash-table))
   ;;  (chunks :initform '())
   (tmp-file-location :accessor tmp-file-location :initarg :tmp-file-location :initform nil)
   (info :accessor info :initform '())))


(defmethod initialize-instance :after ((self content-file) &key)
  (setf (info self)
        (make-instance 'file-upload-info
                       :tmp-location (namestring (tmp-file-location self))
                       :file-type "image/jpg"
                       :content-type "image"
                       :tags (or (gethash "tags" (file-meta self)) #(""))
                       :wants-owned 0)))

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

(defmethod upload-content ((self file-bucket) (session upload-session) (process function))
  ;; NOTE two concurrent uploads of same filename would fail
  ;; give a tmpfile handler to write chunking to
  (funcall process
           (open (tmp-file-location (content-file session))
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
           (lambda (handle)
             (close handle)
             (let ((content (content-file session)))
               (with-foreign-object (result :pointer 2)
                 (finish-file-upload session result (ptr (info content)))
                 (destructuring-bind (fb-id stored-file-id) (coerce (foreign-array-to-lisp result '(:array :string 2)) 'list)
                   ;; (let ((fb-id          (foreign-string-to-lisp c-fb-id))
                   ;;       (stored-file-id (foreign-string-to-lisp c-stored-file-id)))
                   (format t "FileBucket id: ~a StoredFile id: ~a ~%" fb-id stored-file-id)
                   (setf (id content) stored-file-id)
                   (setf (bucket-id content) fb-id)
                   (setf (id self) fb-id)))

               (setf (has-uploaded content) t)
               (setf (uri content)
                     (concatenate 'string config:*hostname* (write-to-string config:*port*)
                                  "/bucket/" (id self) "/" (id content) "/" (gethash "filename" (file-meta content))))
               (uri content)))))

  ;; generate-manifest
  ;; raw-to-dirty-chunks
  ;; dirty-to-clean-chunks
  ;; assign-uri
  ;; (uri content)

;; TODO optionally Distributed task - comslave market

(defmethod raw-to-dirty-chunks ((content content-file) (session upload-session)))
;; (defmethod dirty-to-clean-chunks ((chunks list))
;;   (return cleaned))
(defmethod assign-uri (content content-file))
;; (defmethod get-content-location (content content-file)
;;   (slot-value content uri))
