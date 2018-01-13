(defstruct upload-session ()
  (id)
  (ptr))

(defmethod initialize-instance :around ((self upload-session) &key id)
  (call-next-method self :id id
                         :session-ptr (foreign-funcall "getSession" id)))

(defclass content-file ()
  (id :accessor content-id :initarg :id)
  (uri :accessor uri :initform nil)
  (bucket-id :accessor bucket-id :initarg :bucket-id)
  (is-dirty :accessor is-dirty :initform t)
  (has-uploaded :accessor has-uploaded :initform nil)
  (file-meta :accessor file-meta :initform (make-hash-table :mime-type nil :name ""))
;;  (chunks :initform '())
  (tmp-file-location :accessor))

(defmethod get-content-file-name ((session-ptr :pointer))
  (return (foreign-string-to-lisp (foreign-funcall "getUniqueFileId" session-ptr))))

(defclass file-bucket ()
  ((id :accessor bucket-id :initarg :id)
   (location :accessor location :initarg :location)
   ;; (nameserver )
   ;; you can enabled replication to geo-enabled nodes
   ;; (location )
   (is-foreign :initform nil)))

;; (defmethod download ((ownedSelf bucket)))

(defmethod initialize-instance :around ((self file-bucket))
  (call-next-method self :id (foreign-string-to-lisp (foreign-funcall "getFileBucketId" session-ptr content-type file-type))
                         :location (foreign-string-to-lisp (foreign-funcall "getFileBucketLocation" session-ptr))))

(defmethod upload-content :around ((self bucket) (session upload-session))
  (let ((filename (get-content-file-name (getf session ptr)))
    ;; TODO assure permissions to uplaod to /tmp/ before failing...
    (call-next-method (list :filename filename
                         :mime-type (getf field-headers :content-type)
                            :tmp-file filename)))))

(defmethod upload-content ((list file-info))
  (let (filename (getf file-info filename))
    (defvar content (make-instance content-file :id 1 :bucket-id (getf bucket id)))
    (setf content tmp-file-location filename)

    ;; give a tmpfile handler to write chunking to
    (return (list (open filename
             :if-exists :supersede
             :if-does-not-exist :create
             :direction :output
             :element-type '(unsigned-byte 8))
                  content))))

(defmethod finish-upload-content ((self bucket) (session upload-session) (content content-file))
  (foreign-funcall "finishFileUpload" (getf session session-ptr))
  (setf content has-uploaded t)

  ;; generate-manifest
  ;; raw-to-dirty-chunks
  ;; dirty-to-clean-chunks
  ;; assign-uri
  ;; get-content-location
  )

;; TODO optionally Distributed task - comslave market

(defmethod raw-to-dirty-chunks ((content content-file) (session upload-session)))
(defmethod dirty-to-clean-chunks ((chunks list))
  (return cleaned))
(defmethod assign-uri (content content-file))
(defmethod get-content-location (content content-file)
  (return (getf content-file uri)))

