(defstruct upload-session
  (id nil :read-only t)
  (session-ptr nil :read-only t))

(defmethod initialize-instance :around ((self upload-session) &key id)
  (call-next-method self :id id
                         :session-ptr (foreign-funcall "getSession" id)))

(defclass content-file ()
  ((id :initarg :id)
   (uri :accessor uri :initform nil)
   (bucket-id :reader bucket-id :initarg :bucket-id)
   (is-dirty :accessor is-dirty :initform t)
   (has-uploaded :accessor has-uploaded :initform nil)
   (file-meta :accessor file-meta :initform (make-hash-table))
   ;;  (chunks :initform '())
   (tmp-file-location :accessor tmp-file-location :initform nil)))


;; (defclass video-file (content-file))
;; (defclass audio-file (content-file))


(defmethod get-content-file-name ((session-ptr cffi:foreign-pointer))
  (foreign-string-to-lisp (foreign-funcall "getUniqueFileId" session-ptr)))

(defclass file-bucket ()
  ((id :initarg :id)
   (location :initarg :location)
   ;; (nameserver )
   ;; you can enabled replication to geo-enabled nodes
   ;; (location )
   (is-foreign :initform nil)))

;; (defmethod download ((ownedSelf bucket)))

(defmethod initialize-instance :around ((self file-bucket))
  (call-next-method self :id (foreign-string-to-lisp (foreign-funcall "getFileBucketId" session-ptr content-type file-type))
                         :location (foreign-string-to-lisp (foreign-funcall "getFileBucketLocation" session-ptr))))

(defgeneric upload-content (self session))
(defmethod upload-content ((self file-bucket) (session upload-session))
  (let ((filename (get-content-file-name (session-ptr session))))
    ;; TODO assure permissions to upload to /tmp/ before failing...
    ;; :mime-type (getf field-headers :content-type)
    (call-next-method (list :filename filename
                            :tmp-file filename))))

(defgeneric upload-content (file-info))
(defmethod upload-content ((file-info list))
  ;; TODO don't destructure filename again!
  (let ((filename (getf file-info :filename)))
    (defvar *content* (make-instance content-file :id 1 :bucket-id (slot-value bucket id)))
    (setf (tmp-file-location *content*) filename)

    ;; give a tmpfile handler to write chunking to
    (list :handle (open filename
             :if-exists :supersede
             :if-does-not-exist :create
             :direction :output
             :element-type '(unsigned-byte 8))
          :content *content*)))

(defmethod finish-upload-content ((self file-bucket) (session upload-session) (content content-file))
  (foreign-funcall "finishFileUpload" (session-ptr session))
  (setf (has-uploaded content) t)

  ;; generate-manifest
  ;; raw-to-dirty-chunks
  ;; dirty-to-clean-chunks
  ;; assign-uri
  ;; (uri content)
  )

;; TODO optionally Distributed task - comslave market

(defmethod raw-to-dirty-chunks ((content content-file) (session upload-session)))
;; (defmethod dirty-to-clean-chunks ((chunks list))
;;   (return cleaned))
(defmethod assign-uri (content content-file))
;; (defmethod get-content-location (content content-file)
;;   (slot-value content uri))

