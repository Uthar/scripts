

(defpackage socket
  (:use :cl)
  (:shadow :close)
  (:local-nicknames (:gray :trivial-gray-streams)
                    (:a :alexandria-2)
                    #+abcl (:j :java))
  (:export
   :socket
   :server-socket
   :tls-socket
   :socket-stream
   :socket-input-stream
   :socket-output-stream
   :make-socket
   :make-server-socket
   :make-tls-socket
   :make-socket-input-stream
   :make-socket-output-stream
   :nagle-p
   :socket-close))

(in-package socket)

(defclass socket ()
  ((%socket :initform nil :initarg :socket)))

(defclass server-socket (socket)
  ())

(defclass tls-socket (socket)
  ())

(defun make-socket (host port)
  (make-instance 'socket
    :socket
    #+abcl
    (let* ((address (java:jstatic "getByName" "java.net.InetAddress" host))
           (socket (java:jnew "java.net.Socket" address port)))
      socket)
    #+sbcl
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream
                                 :protocol :tcp)))
      (sb-bsd-sockets:socket-connect
       socket
       (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))
       port)
      socket)))

(defun make-server-socket (port)
  (make-instance 'server-socket
    :socket
    #+abcl
    (java:jnew "java.net.ServerSocket" port)))

(defparameter +tls1.3+ "TLSv1.3")

#+abcl
(defun make-unverified-ssl-context (&optional (proto +tls1.3+))
  (let* ((ssl-context
           (java:jstatic "getInstance" "javax.net.ssl.SSLContext" proto))
         (trm (java:jinterface-implementation
               "javax.net.ssl.X509TrustManager"
               "getAcceptedIssuers" (constantly nil)
               "checkClientTrusted" (constantly (values))
               "checkServerTrusted" (constantly (values))))
         (trms (java:jnew-array-from-list
                "javax.net.ssl.X509TrustManager"
                (list trm))))
    (java:jcall "init" ssl-context java:+null+ trms java:+null+)
    ssl-context))

#+abcl
(defun make-default-ssl-context (&optional (proto +tls1.3+))
  (let* ((ssl-context
           (java:jstatic "getInstance" "javax.net.ssl.SSLContext" proto)))
    (java:jcall "init" ssl-context java:+null+ java:+null+ java:+null+)
    ssl-context))

(defun make-tls-socket (host port &optional (proto +tls1.3+)
                                            (verify t))
  #+abcl
  (handler-case
      (make-instance
       'tls-socket
       :socket
       (let* ((address (java:jstatic "getByName" "java.net.InetAddress" host))
              (context (if verify
                           (make-default-ssl-context proto)
                           (make-unverified-ssl-context proto)))
              (factory (java:jcall "getSocketFactory" context))
              (socket (java:jcall "createSocket" factory address port)))
         socket))
    (java:java-exception (e)
      (error 'end-of-file e))))

;; Test: ncat --listen --ssl 5555
;; (defparameter socket (make-tls-socket "localhost" 5555 +tls1.3+ nil))
;; (defparameter out (make-socket-output-stream socket))
;; (defparameter in (make-socket-input-stream socket))
;; (write-sequence (encode::string->octets "hello world") out)


(defun make-socket-input-stream (socket)
  (make-instance
   'socket-input-stream
   :socket socket
   :stream
   #+abcl
   (handler-case
       (java:jcall "getInputStream" (slot-value socket '%socket))
     (java:java-exception (e)
       (socket:socket-close socket)
       (error 'end-of-file)))
   #+sbcl
   (sb-bsd-sockets:socket-make-stream (slot-value socket '%socket)
                                      :input t
                                      :output t
                                      :element-type '(unsigned-byte 8)
                                      :buffering :none
                                      :auto-close nil)))
                
(defun make-socket-output-stream (socket)
  (make-instance
   'socket-output-stream
   :socket socket
   :stream
   #+abcl
   (handler-case
       (java:jcall "getOutputStream" (slot-value socket '%socket))
     (java:java-exception (e)
       (socket:socket-close socket)
       (error 'end-of-file)))
   #+sbcl
   (sb-bsd-sockets:socket-make-stream (slot-value socket '%socket)
                                      :input t
                                      :output t
                                      :element-type '(unsigned-byte 8)
                                      :buffering :none
                                      :auto-close nil)))

(defclass socket-stream (gray:fundamental-binary-stream)
  ((socket :initarg :socket :initform (error "socket required"))
   (%stream :initarg :stream :initform (error "stream required"))))

(defclass socket-input-stream (socket-stream
                               gray:fundamental-binary-input-stream)
  ())

(defclass socket-output-stream (socket-stream
                                gray:fundamental-binary-output-stream)
  ())


(defmethod gray:stream-read-byte ((stream socket-input-stream))
  (with-slots (%stream) stream
    #+abcl
    (handler-case
        (let ((read (java:jcall "read" %stream)))
          (when (= read -1)
            (socket:socket-close stream)
            (error 'end-of-file))
          read)
      (java:java-exception (e)
        (socket:socket-close stream)
        (error 'end-of-file)))
    #+sbcl
    (read-byte %stream)))

(defmethod gray:stream-write-byte ((stream socket-output-stream) integer)
  (with-slots (%stream) stream
    #+abcl (error "Not implemented")
    #+sbcl
    (write-byte integer %stream)))

(defmethod gray:stream-read-sequence ((stream socket-input-stream)
                                      sequence start end &key)
  (with-slots (%stream) stream
    #+abcl
    (handler-case
        (let* ((buf (java:jnew-array "byte" (length sequence)))
               (read (java:jcall "read" %stream buf start (- (or
                                                              end
                                                              (length sequence))
                                                             start))))
          (if (= read -1)
              (prog1 0 (socket:socket-close stream) (error 'end-of-file))
              ;; 0
              (loop for index below read
                    do (setf (elt sequence (+ start index))
                             (java:jarray-ref buf (+ start index)))
                    finally (return (+ start read)))))
      (java:java-exception (e)
        (socket:socket-close stream)
        (error 'end-of-file)))
    #+sbcl
    (read-sequence sequence %stream :start start :end end)))


(defmethod gray:stream-write-sequence ((stream socket-output-stream)
                                       sequence start end &key)
  (with-slots (%stream) stream
    #+abcl
    (handler-case
        (let ((jarray (java:jnew-array-from-list "byte"
                                                 (coerce sequence 'list))))
          (java:jcall "write" %stream jarray start (or end (length sequence)))
          sequence)
      (java:java-exception (e)
        (socket:socket-close stream)
        (error 'end-of-file)))
    #+sbcl
    (write-sequence sequence %stream :start start :end end)))

(defmethod socket-close ((socket socket))
  #+abcl (java:jcall "close" (slot-value socket '%socket))
  #+sbcl (sb-bsd-sockets:socket-close (slot-value socket '%socket)))

(defmethod socket-close ((stream socket-stream))
  #+abcl (java:jcall "close" (slot-value stream '%stream))
  #+sbcl (sb-bsd-sockets:socket-close (a:line-up-first
                                       stream
                                       (slot-value 'socket)
                                       (slot-value '%socket))))

(defmethod socket-close :before (stream)
  (format t "Closing ~a~%" stream))

(defmethod nagle-p ((socket socket))
  #+abcl
  (not (java:jcall "getTcpNoDelay" (slot-value socket '%socket)))
  #+sbcl
  (not (sb-bsd-sockets:sockopt-tcp-nodelay (slot-value socket '%socket))))

(defmethod (setf nagle-p) (nagle-p (socket socket))
  (assert (or (eql nil nagle-p) (eql t nagle-p)))
  #+abcl
  (java:jcall "setTcpNoDelay" (slot-value socket '%socket) (not nagle-p))
  #+sbcl
  (setf (sb-bsd-sockets:sockopt-tcp-nodelay (slot-value socket '%socket))
        (not nagle-p)))
