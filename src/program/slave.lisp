(defpackage :slave
  (:use :common-lisp :cffi :alexandria :filebucket :blackbird :wookie :wookie-plugin-export)
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

(defun json-body (body &optional (stream (make-string-output-stream)))
  (yason:encode body stream)
  stream)

(defmacro send-json-response (res &key headers body other)
  `(let ((output (get-output-stream-string (json-body ,body))))
     (send-response ,res :headers (append ,headers '(:access-control-allow-origin "*" :content-type "application/json"))
                    :body output
                    ,@other)))

(defroute (:options "/prepare-upload/") (req res)
  (send-response res :headers '(:access-control-allow-origin "*" :access-control-allow-headers "X-Accept-Charset, X-Accept, Accept, Accept-Encoding, Authorization, Content-Type, Dnt, Origin, User-Agent, X-Requested-With" :access-control-max-age="86400" :access-control-allow-methods "OPTIONS, POST" :content-type="application/json" :connection "keep-alive" :pragma "no-cache" :cache-control "no-store, no-cache, must-revalidate") :status 204))


(defroute (:options "/upload-chunked/SESSION/([0-9]+)") (req res)
  (send-response res :headers '(:access-control-allow-origin "*" :access-control-allow-headers "X-Accept-Charset, X-Accept, Accept, Accept-Encoding, Authorization, Content-Type, Dnt, Origin, User-Agent, X-Requested-With" :access-control-max-age="86400" :access-control-allow-methods "OPTIONS, POST" :content-type="application/json" :connection "keep-alive" :pragma "no-cache" :cache-control "no-store, no-cache, must-revalidate" :transfer-encoding "chunked") :status 204))


;; Content-Type: text/html; charset=utf-8
;; Access-Control-Allow-Origin: http://localhost:8080
;; Vary: Origin
;; Access-Control-Allow-Headers: accept, accept-encoding, authorization, content-type, dnt, origin, user-agent, x-csrftoken, x-requested-with
;; Access-Control-Allow-Methods: DELETE, GET, OPTIONS, PATCH, POST, PUT
;; Access-Control-Max-Age: 86400


;; Web worker will call this for each file upload
(defroute (:post "/prepare-upload/") (req res)
  ;; prepare a session for an incoming upload
  ;;
  (let ((body-bytes (request-body req)))
    (let ((req-data (if body-bytes (yason:parse (babel:octets-to-string body-bytes :encoding :utf-8)))))
      (if req-data
          ;; TODO test for proper json fields, etc.
          (let ((session-id (intern (string (gensym "SESSION-"))))
                (response-data (make-hash-table)))
            (setf (gethash session-id *upload-sessions*)
                  ;; TODO sanitize file-meta
                  (make-instance 'upload-session :id session-id :content-file (make-instance 'content-file :tmp-file-location (merge-pathnames config:*tmp-location* (pathname (gethash "filename" req-data))) :file-meta req-data)))

            (setf (gethash "id" response-data) (subseq (string session-id) 8))
            (send-json-response res :body response-data))

          (send-json-response res :body (plist-hash-table '("error" "invalid-json" "message" "Invalid JSON"))
            :other (:status 400))))))
    ;; TODO check if req-data is JSON

(defroute (:post "/upload-chunked/SESSION/([0-9]+)" :chunk t :suppress-100 t) (req res args)

  (add-hook :pre-route
    (lambda (req res)
      (with-promise (resolve reject)
        (let* ((session-id (concatenate 'string "SESSION-" (car args)))
               (session (gethash (intern session-id) *upload-sessions*)))
          (if (and session (not (in-progress session)))
              (progn
                (setf (request-data req) session)
                (resolve))
              (progn
                (send-json-response
                 res
                 :body (plist-hash-table '("error" "no-session" "message" "Upload session expired or does not exist"))
                 :headers '(:content-type "text/json") :other (:status 400))
                (reject (make-instance 'error))
                     ))))))

  (setf (request-store-body req) nil)
  ;; TODO before route: verify permission for this psession id
  (let ((session (request-data req)))
    ;; when we receive a Expect: 100-continue header, send back the please continue header after receiving a file upload session
    ;; (when (string= (gethash "transfer-encoding" (request-headers req))
    ;;                "chunked")
    ;;   ;; TODO error handling
    ;;   ;;(if bucket-id nil (raise-no-buckets))
    ;;   (pprint "100-continue")
    ;;   (send-100-continue res))
    (setf (in-progress session) t)

    (let ((bucket (get-bucket session (ptr (info (content-file session))))))
      ;; TODO streaming with XHR
      ;; (if token (token upload-session))
      (upload-content
       bucket session
       (lambda (handle finish-upload)
         ;; upload file to bucket location chunk by chunk
         (format t "handling upload...")
         (with-chunking req (chunk last-chunk-p)
           (write-sequence chunk handle)
           (force-output handle)

           (when last-chunk-p
             ;; clear the session once finished
             (format t "completed chunked upload, finishing...")
             (send-json-response res :body (funcall finish-upload handle))
             (delete-ptr session)
             (remhash (slot-value session 'id) *upload-sessions*))))))))


(defun run-dev-server ()
  ;; ;; Development configuration
  (let ((blackbird:*debug-on-error* nil)
        (wookie-config:*debug-on-error* nil))

    (ensure-directories-exist config:*tmp-location*)

    ;; Tried to handle this error, but it doesn't work
    (as:with-event-loop
          ()
        (vom:config :wookie :debug1)
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
