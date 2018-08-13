(defpackage :slave
  (:use :common-lisp :cffi :cl-ppcre :alexandria :filebucket :blackbird :wookie :wookie-plugin-export :cffi-utils)
  (:export #:run-dev-server))

(in-package :slave)

;; loads Wookie core plugins
(load-plugins)

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


(add-hook :pre-route
  (lambda (req res)
    (when (search "/upload-chunked/SESSION" (request-resource req))
      (cl-ppcre:register-groups-bind (session-id)
          ("/upload-chunked/SESSION/([0-9]+)" (request-resource req))
        (let ((session (gethash (intern (concatenate 'string "SESSION-" session-id)) *upload-sessions*)))
          (if (and session (not (in-progress session)))
            (progn
              (format t "PRE-ROUTE session: ~a~%" session)
              (setf (request-data req) session))
            (progn
              (send-json-response
               res
               :body (plist-hash-table '("error" "no-session" "message" "Upload session expired or does not exist"))
               :headers '(:content-type "text/json") :other (:status 400 :close t))
              (make-instance 'error))))))))

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
                  (make-instance 'upload-session
                    :id session-id
                    :content-file (make-instance 'uploaded-content-file
                                    :tmp-file-location (merge-pathnames config:*tmp-location* (pathname (gethash "filename" req-data)))
                                    :file-meta req-data)))

            (setf (gethash "id" response-data) (subseq (string session-id) 8))
            (send-json-response res :body response-data))

          (send-json-response res :body (plist-hash-table '("error" "invalid-json" "message" "Invalid JSON"))
            :other (:status 400))))))
    ;; TODO check if req-data is JSON

(defroute (:post "/upload-chunked/SESSION/([0-9]+)" :chunk t :suppress-100 t) (req res args)
  ;; TODO before route: verify permission for this psession id
  (let ((session (request-data req)))
    (when session
      (format t "SESSION: ~a ~%" session)
      (setf (in-progress session) t)

      ;; TODO when we receive a Expect: 100-continue header, send back the please continue header after receiving a file upload session
      ;; (when (string= (gethash "transfer-encoding" (request-headers req))
      ;;                "chunked")
      (let ((bucket (fetch-bucket session (ptr (info (content-file session))))))
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
               (remhash (intern (car args)) *upload-sessions*)))))))))

;;               bucket id           file id
;;               base 32 id          base 32 id        filename
(defroute (:get "/([0-9A-Z\*\~\$=]+)/([0-9A-Z\*\~\$=]+)/([a-zA-Z0-9\s\._-]+)") (req res args)
  (destructuring-bind (bucket-id content-id filename) args
    (with-foreign-class (hosting-session
                         :id (gensym "HSESSION-")
                         :in-progress t
                         :content-file (make-instance 'hosted-content-file :id (b32c:b32c-decode content-id) :file-name filename))
      session
      (let ((bucket (get-bucket session (b32c:b32c-decode bucket-id))))
        (if (or (eql (bucket-id bucket) -1)
                (not (eql (get-content-file session (ptr (info (content-file session)))) 1)))
            (send-response res :status 400 :body "Invalid bucket or file")

            (let* ((res-headers (list :content-type (gethash "type" (file-meta (content-file session)))
                                      :cache-control "public, max-age=31536000"))
                   (stream (start-response res :headers res-headers)))
              ;; TODO make this use promises!!!
              (host-content
               bucket session
               (lambda (chunk)
                 ;; Begin chunking procedure here
                 (format t "Writing chunk!~%~%")
                 (write-sequence chunk stream)
                 (format t "Wrote chunk!~%")))
              (finish-response res :close t)))))))

(defun run-dev-server ()
  ;; ;; Development configuration
  (let ((blackbird:*debug-on-error* t)
        (wookie-config:*debug-on-error* t))

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
           4
           (lambda (sig)
             (declare (ignore sig))
             ;; remove the signal handler to end the event loop
             (as:free-signal-handler 4)
             ;; graceful stop
             (as:close-tcp-server server)))))))
