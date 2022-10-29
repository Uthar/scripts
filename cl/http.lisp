

(defpackage dev.cadm.http
  (:documentation "RFC9110 client implementation")
  (:use :cl :alexandria)
  (:export :request))

;; Authorization schemes:
;; https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml


(in-package dev.cadm.http)

;; TODO gray streams

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

(defun request (params)
  (let* ((host (assoc* 'host params))
         (port (assoc* 'port params))
         (address (java:jstatic "getByName" "java.net.InetAddress" host))
         (socket (java:jnew "java.net.Socket" address port))
         (out (java:jcall "getOutputStream" socket))
         (in (java:jcall "getInputStream" socket))
         (payload (params->payload params)))
    (java:jcall "write" out (string->jarray payload))
    (java:jcall "flush" out)
    (read-http-response in)))

(defun is-read-line (is)
  (loop
    for byte = (java:jcall "read" is)
    while (not (or (char= (code-char byte) #\Return)
                   (char= (code-char byte) #\Newline)))
    collect byte into bytes
    finally (return (prog1 (bytes->string bytes)
                      (java:jcall "skip" is 1)))))

(defun read-headers (is)
  (loop
    for line = (is-read-line is)
    while (not (string= "" line))
    for (name value) = (uiop:split-string line :separator ":")
    collect (cons name (string-trim " " value))))

;; FIXME not java specific
(defun read-http-response (is)
  (let* ((control-data (is-read-line is))
         (headers (read-headers is))
         (content-length (parse-integer
                          (assoc* "content-length" headers
                                 :key #'string-downcase
                                 :test #'string=)
                          :junk-allowed t)))
    (if content-length
        (let ((response (java:jnew-array "byte" content-length)))
          (jcall "read" is response)
          (prog1 (jarray->string response)
            (jcall "close" is)))
        (error "not implemented: HTTP requests with no content-length"))))
                                          
(defun |InputStream->string| (is)
  (loop
    with out = (java:jnew "java.io.ByteArrayOutputStream")
    with buffer = (java:jnew-array "byte" 4096)
    for read = (java:jcall "read" is buffer)
    when (plusp read)
      do (java:jcall "write" out buffer 0 read)
    when (not (plusp read))
      do (return (java:jcall "toString" out "UTF-8"))))

(defun string->bytes (string)
  (make-array (length string)
              :element-type '(unsigned-byte 8)
              :initial-contents (map 'vector #'char-code string)))

(defun bytes->string (bytes)
  (make-array (length bytes)
              :element-type 'character
              :initial-contents (map 'vector #'code-char bytes)))

;; TODO babel
(defun string->jarray (string)
  (java:jnew-array-from-array "byte" (string->bytes string)))

(defun jarray->string (jarray)
  (loop for i from 0 below (java:jarray-length jarray)
        collecting (java:jarray-ref jarray i) into list
        finally (return (make-array (length list)
                                    :element-type 'character
                                    :initial-contents
                                    (map 'vector #'code-char list)))))

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


