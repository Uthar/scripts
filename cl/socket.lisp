

(defpackage dev.cadm.socket
  (:use :cl)
  (:shadow :close)
  (:local-nicknames (:gray :trivial-gray-streams)
                    (:a :alexandria-2)
                    (:b :babel))
  (:export
   :socket
   :socket-stream
   :make-socket
   :socket-input-stream
   :socket-output-stream
   :close
   :bind
   :connect
   :closed-p
   :connected-p
   :bound-p))

(in-package dev.cadm.socket)

(defclass socket ()
  ((%socket :initform nil :initarg :socket)))

(defclass server-socket (socket)
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
    #+abcl (java:jcall "read" %stream)))

(defmethod gray:stream-read-sequence ((stream socket-input-stream)
                                      sequence start end &key)
  (with-slots (%stream) stream
    #+abcl
    (let* ((buf (java:jnew-array "byte" (length sequence)))
           (read (java:jcall "read" %stream buf start (or
                                                       end
                                                       (length sequence)))))
      (loop for index below read
            do (setf (elt sequence (+ start index))
                     (java:jarray-ref buf (+ start index))))
      read)))


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
