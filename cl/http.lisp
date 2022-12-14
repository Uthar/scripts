
(defpackage http
  (:documentation "RFC9110 client implementation")
  (:use :cl :alexandria)
  (:import-from :split-sequence :split-sequence)
  (:shadowing-import-from :concurrent :gethash :clrhash)
  (:import-from :concurrent :make-concurrent-hash-table)
  (:export
   :request
   :request*))

;; Authorization schemes:
;; https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml


(in-package http)

;; HTTP/1.1 messages contain a potentially unbounded stream of content (body)

;; 6.  Message Abstraction

;; TODO handle trailers
;;
;; Framing and control data is sent first, followed by a header section
;; containing fields for the headers table.  When a message includes
;; content, the content is sent after the header section, potentially
;; followed by a trailer section that might contain fields for the
;; trailers table.
;;
;; Messages are expected to be processed as a stream, wherein the
;; purpose of that stream and its continued processing is revealed while
;; being read.  Hence, control data describes what the recipient needs
;; to know immediately, header fields describe what needs to be known
;; before receiving content, the content (when present) presumably
;; contains what the recipient wants or needs to fulfill the message
;; semantics, and trailer fields provide optional metadata that was
;; unknown prior to sending the content.

;; Use TCP:
;; HTTP is a client/server protocol that operates over a reliable
;; transport- or session-layer "connection".

;; 6.2.  Control Data
;; control data is sent as the first line of a message

(defvar *connections*
  (make-concurrent-hash-table))

(defun cache-connection (connection &rest cache-keys)
  (let ((cache-key (apply #'concatenate 'string
                          (interpose 'list "-" cache-keys))))
    (setf (gethash cache-key *connections*) connection)))

(defun get-cached-connection (&rest cache-keys)
  (let ((cache-key (apply #'concatenate 'string
                          (interpose 'list "-" cache-keys))))
    (format t "trying to reuse connection ~A~%" cache-key)
    (when-let ((conn (gethash cache-key *connections*)))
      (format t "reusing cached connection ~A~%" conn)
      conn)))
  
(defun request (params)
  (let* ((host (assoc* 'host params))
         (port (assoc* 'port params))
         (proto (assoc* 'proto params))
         (socket (or
                  (get-cached-connection
                   host
                   (write-to-string port)
                   (write-to-string proto))
                  (case proto
                   (http (socket:make-socket host port))
                   (https (socket:make-tls-socket host port))
                   (otherwise (socket:make-socket host port)))))
         (out (socket:make-socket-output-stream socket))
         (in (socket:make-socket-input-stream socket))
         (payload (params->payload params)))
    (setf (socket:nagle-p socket) nil)
    (write-sequence (encode:string->octets payload) out)
    (read-http-response in)))

(defun strassoc (&rest args)
  (apply #'assoc `(,@args :test ,#'equalp)))

(defun strassoc* (&rest args)
  (cdr (apply #'strassoc args)))

(defparameter *retries* 0)

(defun call-with-retrying (func &rest args)
  (restart-case
      (handler-case
          (apply func args)
        (error (e)
          (clrhash *connections*)
          (when (< *retries* 5)
            (invoke-restart 'retry e args))))
    (retry (c args)
      (sleep (* 3 *retries*))
      (let ((*retries* (1+ *retries*)))
        (format t "Retry x~a after ~a~%" *retries* c)
        (apply #'call-with-retrying func args)))))

(defun request* (params)
  ;; TODO(kasper): should handle Content-Length
  ;; TODO(kasper): should handle Content-Encoding
  ;; TODO(kasper): streaming encode
  (multiple-value-bind (stream code reason headers version)
      (request params)
    (let* ((transfer-encoding (strassoc* "Transfer-Encoding" headers))
           (chunkedp (equalp transfer-encoding "chunked"))
           (content-length (strassoc* "Content-Length" headers))
           (connection (strassoc* "Connection" headers))
           (keep-alive-p (equalp connection "keep-alive"))
           (method (assoc* 'method params)))
      (assert (string= version +http/1.1+))
      (assert (not (and content-length chunkedp)))
      (when keep-alive-p
        (cache-connection (slot-value stream 'socket::socket)
                          (assoc-value params 'host)
                          (write-to-string (assoc-value params 'port))
                          (write-to-string (assoc-value params 'proto))))
      (cond
        ((string= method "HEAD") "")
        (chunkedp (read-chunks stream))
        (content-length (read-content-length-response
                         stream
                         (parse-integer content-length)))
        (t (read-response stream))))))
    
(defun read-chunks (stream)
  (loop with whole = (make-byte-array 0 :adjustable t)
        for line = (is-read-line stream)
        for chunk-length = (parse-integer
                            (string-trim " " (first (split-sequence #\; line)))
                            :radix 16)
        while (plusp chunk-length)
        for chunk = (read-chunk stream chunk-length)
        do (let ((position (length whole)))
             (adjust-array whole (+ (length whole)
                                    (length chunk)))
             (replace whole chunk :start1 position))
        finally (return (prog1 (encode:octets->string whole)
                          ;; Check for trailers
                          (unless (string= "" (is-read-line stream))
                            (read-trailers stream)
                            (is-read-line stream))))))

(defun read-trailers (stream)
  (loop 
    for trailer = (is-read-line stream)
    collecting trailer into trailers
    if (string= trailer "")
      do (return (butlast trailers))))

(defmacro while (test &body body)
  `(loop while ,test do (progn ,@body)))

(defun read-chunk (stream length)
  (let* ((chunk (make-byte-array length))
         (position (read-sequence chunk stream)))
    (while (< position length)
      (setf position (read-sequence chunk stream :start position)))
    ;; Skip CRLF
    (is-read-line stream)
    chunk))

(defun read-content-length-response (stream length)
  (let* ((response (make-byte-array length))
         (position (read-sequence response stream)))
    (while (< position length)
      (setf position (read-sequence response stream :start position)))
    (encode:octets->string response)))

(defun read-response (stream)
  (encode:octets->string (read-stream-content-into-byte-vector stream)))
  
(defun is-read-line (is)
  (loop
    for byte = (read-byte is)
    while (not (or (char= (code-char byte) #\Return)
                   (char= (code-char byte) #\Newline)))
    collect byte into bytes
    finally (return (prog1 (encode:octets->string
                            (coerce bytes '(vector (unsigned-byte 8))))
                      (read-byte is)))))

(defun read-headers (is)
  (loop
    for line = (is-read-line is)
    while (not (string= "" line))
    for (name value) = (uiop:split-string line :separator ":")
    collect (cons name (string-trim " " value))))

(defun make-byte-array (&rest args)
  (apply #'make-array `(,@args :element-type (unsigned-byte 8))))

(defun read-http-response (is)
  (let* ((control-data (is-read-line is))
         (headers (read-headers is))
         (splits (split-sequence #\Space control-data))
         (version (first splits))
         (code (second splits))
         (reason (nthcdr 2 splits)))
    ;; TODO(kasper): handle chunked encoding
    (values is
            (parse-integer code)
            (apply #'concatenate 'string (interpose 'list " " reason))
            headers
            version)))

(defun interpose (result-type separator sequence)
  (coerce
   (reduce (lambda (sequence item)
             (if (null sequence)
                 (list* item sequence)
                 (list* item separator sequence)))
           (reverse sequence)
           :initial-value nil)
   result-type))

(defun string->bytes (string)
  (make-array (length string)
              :element-type '(unsigned-byte 8)
              :initial-contents (map 'vector #'char-code string)))

(defun bytes->string (bytes)
  (make-array (length bytes)
              :element-type 'character
              :initial-contents (map 'vector #'code-char bytes)))

(defparameter +http/1.1+
  "HTTP/1.1")

(defparameter +space+
  " ")

(defparameter +crlf+
  (make-array 2 :element-type 'character
                :initial-contents '(#\Return #\Newline)))

(defun headers->string (headers)
  (loop
    for (name . value) in headers
    collect (list name ": " value +crlf+) into strings
    finally (return (apply #'concatenate 'string (flatten strings)))))

(defun assoc* (&rest args)
  (cdr (apply #'assoc args)))

(defun params->payload (params)
  "RFC1945, Section 4."
  (concatenate 'string
    (assoc* 'method params) +space+
    (assoc* 'path params) +space+
    +http/1.1+ +crlf+
    (headers->string (assoc* 'headers params))
    +crlf+
    (assoc* 'body params)))

;; A user agent MUST generate a Host header field in a request
;; unless it sends that information as an ":authority" pseudo-header
;; field.  A user agent that sends Host SHOULD send it as the first
;; field in the header section of a request.

;; All general-purpose servers MUST support the methods GET and HEAD.
;; All other methods are OPTIONAL.

(defun test-request ()
  (let* ((params `((host . "example.org")
                   (port . 80)
                   (method . "GET")
                   (headers . (("Host" . "example.org:80")
                               ("Accept" . "text/html")
                               ("Accept-Charset" . "utf-8")
                               ("Accept-Encoding" . "identity")))
                   (path . "/index.html")))
         (response (request params)))
    response))

;; (defparameter response (test-request))
