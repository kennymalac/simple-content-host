;; (in-package :simple-content-host)

;; loads Wookie core plugins
(load-plugins)

;; directory route for assets (dev-mode)
(def-directory-route "/" "./assets")

(defun write-file-chunk (out-buffer chunk)
  ;; write chunk to file
  )


;; assemble chunks
;; chunk a7sDSX230smu -> chunk 30AcmyuSdnart23 -> chunk sm9RAR90s9lSe -> (single file)

;; Web worker will call this for each file upload
(defroute (:post "/prepare-upload") (req resp)
  ;; pseudo-code

  ;; prepare a session for an incoming upload
  ;;  
  (send-response res :body "session id")
  )

(defroute (:post "/upload-chunked/([0-9]+)" :chunk t :suppress-100 t) (req resp)

;; before route: verify permission for this session id
  ;; multipart - once we receive metadata, use it
  ;; pretend that the client is not lying about the contents, verification is expensive
  ;; TODO actual parameters
  (defparameter content-type (foreign-string-alloc "IMAGE"))
  (defparameter file-type (foreign-string-alloc "png"))
  (defvar bucket-id nil)

  ;; when we receive a Expect: 100-continue header, send back the please continue header after receiving a file upload session
  (when (string= (gethash "expect" (request-headers req))
                 "100-continue")
    (alet ()
      ;; TODO error handling
      ;;(if bucket-id nil (raise-no-buckets))
          
      (send-100-continue resp)))

  (let* ((session (make-instance upload-session :id 2))
         (bucket (make-instance bucket))
         ;; TODO destructuring bind to get handle AND content file
         (destructuring-bind (handle content)
             `(upload-content bucket session)
           ((with-chunking (chunk last-chunk-p)
              ;; upload file to bucket location chunk by chunk
              (write-sequence body-bytes handle)
              (force-output (handle))

              (if last-chunk-p
                  (progn
                    (finish-upload-content bucket session content)
                    (close handle)
                    ))))))

  ;; SEND processing event
  (send-response res :body "Test"))


;; Development configuration
(let ((blackbird:*debug-on-error* t)
      (wookie-config:*debug-on-error* t))

(as:with-event-loop
 ()
 ;; create a listener, and pass it to start-server, starting Wookie
 (let* ((listener (make-instance 'listener
                                 :bind "127.0.0.1"
                                 :port 4242))
        ;; start the http server (this passes back a cl-async server class)
        (server (start-server listener)))
   ;; stop server on ctrl+c
   (as:signal-handler
    2
    (lambda (sig)
             (declare (ignore sig))
             ;; remove the signal handler to end the event loop
             (as:free-signal-handler 2)
             ;; graceful stop
             (as:close-tcp-server server)))))
