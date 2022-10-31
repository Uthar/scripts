

(defpackage socket
  (:use :cl)
  (:shadow :close)
  (:local-nicknames (:gray :trivial-gray-streams)
                    (:a :alexandria-2)
                    #+abcl (:j :java))
  (:export
   :socket
   :socket-stream
   :make-socket
   :make-tls-socket
   :socket-input-stream
   :socket-output-stream
   :make-socket-input-stream
   :make-socket-output-stream
   :close
   :bind
   :connect
   :closed-p
   :connected-p
   :bound-p))

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
      socket)))

(defun make-server-socket (port)
  (make-instance 'server-socket
    :socket
    #+abcl
    (java:jnew "java.net.ServerSocket" port)))

(defparameter +tls1.3+ "TLSv1.3")

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

(defun make-default-ssl-context (&optional (proto +tls1.3+))
  (let* ((ssl-context
           (java:jstatic "getInstance" "javax.net.ssl.SSLContext" proto)))
    (java:jcall "init" ssl-context java:+null+ java:+null+ java:+null+)
    ssl-context))

(defun make-tls-socket (host port &optional (proto +tls1.3+)
                                            (verify t))
  (make-instance 'tls-socket
    :socket
    #+abcl
    (let* ((address (java:jstatic "getByName" "java.net.InetAddress" host))
           (context (if verify
                        (make-default-ssl-context proto)
                        (make-unverified-ssl-context proto)))
           (factory (java:jcall "getSocketFactory" context))
           (socket (java:jcall "createSocket" factory address port)))
      socket)))

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
   (java:jcall "getInputStream" (slot-value socket '%socket))))
                
(defun make-socket-output-stream (socket)
  (make-instance
   'socket-output-stream
   :socket socket
   :stream
   #+abcl
   (java:jcall "getOutputStream" (slot-value socket '%socket))))

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
    #+abcl (let ((read (java:jcall "read" %stream)))
             (when (= read -1)
               (close stream)
               (error 'end-of-file))
             read)))

(defmethod gray:stream-read-sequence ((stream socket-input-stream)
                                      sequence start end &key)
  (with-slots (%stream) stream
    #+abcl
    (let* ((buf (java:jnew-array "byte" (length sequence)))
           (read (java:jcall "read" %stream buf start (- (or end
                                                             (length sequence))
                                                         start))))
      (format t "read ~A bytes~%" read)
      ;; (when (< (+ start read) (length sequence))
      ;;   (close stream))
      (if (= read -1)
          (prog1 0 (close stream) (error 'end-of-file))
          ;; 0
          (loop for index below read
                do (setf (elt sequence (+ start index))
                         (java:jarray-ref buf (+ start index)))
                finally (return (+ start read)))))))


(defmethod gray:stream-write-sequence ((stream socket-output-stream)
                                       sequence start end &key)
  (with-slots (%stream) stream
    #+abcl
    (let ((jarray (java:jnew-array-from-list "byte" (coerce sequence 'list))))
      (java:jcall "write" %stream jarray start (or end (length sequence)))
      sequence)))

(defmethod accept ((server server-socket))
  (make-instance
   'socket
   :socket
   #+abcl (java:jcall "accept" (slot-value server '%socket))))

(defmethod close ((socket socket))
  #+abcl (java:jcall "close" (slot-value socket '%socket)))

(defmethod close ((stream socket-stream))
  #+abcl (java:jcall "close" (slot-value stream '%stream)))

(defmethod close :before (stream)
  (format t "Closing ~a~%" stream))
