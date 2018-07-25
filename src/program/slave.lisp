(defpackage :slave
  (:use :common-lisp :cffi :alexandria :filebucket :wookie :wookie-plugin-export)
  (:export #:run-dev-server))

(in-package :slave)

;; loads Wookie core plugins
(load-plugins)

;; directory route for assets (dev-mode)
;;(def-directory-route "/" "./assets")

;; (defun write-file-chunk (out-buffer chunk)
;;   ;; write chunk to file
;;   )

;; (add-hook
;;  :parsed-headers
;;  (lambda (request)
;;    (setf (http-parse:http-store-body (request-http request)) t)))

(defparameter *upload-sessions* (make-hash-table))
;; assemble chunks
;; example:
;; chunk a7sDSX230smu -> chunk 30AcmyuSdnart23 -> chunk sm9RAR90s9lSe -> (single file)

(defmacro send-json-response (res &key headers body other)
  `(let ((s (make-string-output-stream)))
     (yason:encode ,body s)
     (send-response ,res :headers (append ,headers '(:content-type "application/json"))
                    :body (get-output-stream-string s)
                    ,@other)))

;; Web worker will call this for each file upload
(defroute (:post "/prepare-upload") (req res)
  ;; prepare a session for an incoming upload
  ;;
  (let ((body-bytes (request-body req)))
    (let ((req-data (if body-bytes (yason:parse (babel:octets-to-string body-bytes :encoding :utf-8)))))
      (if req-data
          ;; TODO test for proper json fields, etc.
          (let ((session-id (gensym "SESSION-"))
                (response-data (make-hash-table)))
            (setf (gethash session-id *upload-sessions*)
                  (make-instance 'upload-session :content-file (make-instance 'content-file :file-meta req-data)))
            (setf (gethash "id" response-data) (string session-id))
            (send-json-response res :body response-data))

          (send-json-response res :body (plist-hash-table '("error" "invalid-json" "message" "Invalid JSON"))
            :other (:status 400))))))
    ;; TODO check if req-data is JSON

(defroute (:post "/upload-chunked/(SESSION-[0-9]+)" :chunk t :suppress-100 t) (req res args)
  ;; TODO before route: verify permission for this session id


  (let* ((session-id (car args))
         (session    (gethash session-id *upload-sessions*)))
    (unless session
      (let ((error-response-data (make-hash-table)))
        (setf (gethash 'message error-response-data) "Upload session expired or does not exist")
        (setf (gethash 'error error-response-data) "no-session")
        (send-json-response res :body error-response-data)))

    ;; when we receive a Expect: 100-continue header, send back the please continue header after receiving a file upload session
    (when (string= (gethash "expect" (request-headers req))
                   "100-continue")
      ;; TODO error handling
      ;;(if bucket-id nil (raise-no-buckets))
      (send-100-continue res))

    (with-upload-session session
      (let ((bucket (make-instance 'file-bucket)))
        (send-json-response res :body (serialize (upload-content bucket session nil
          (lambda (handle)
           ;; upload file to bucket location chunk by chunk
            (with-chunking req (chunk last-chunk-p)
              (write-sequence chunk handle)
              (force-output handle)

              (when last-chunk-p
                ;; clear the session once finished
                (remhash session-id *upload-sessions*)))))))))))


(defun run-dev-server ()
  ;; ;; Development configuration
  (let ((blackbird:*debug-on-error* t)
        (wookie-config:*debug-on-error* t))

    (as:with-event-loop
        ()
      ;; create a listener, and pass it to start-server, starting Wookie
      (let* ((listener (make-instance 'listener
                                      :bind config:*hostname*
                                      :port config:*port*))
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
           (as:close-tcp-server server)))))))
