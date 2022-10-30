(defpackage tls
  (:use :common-lisp)
  (:import-from :split-sequence :split-sequence)
  (:local-nicknames (:a :alexandria-2)
                    (:b :babel)
                    (:s :dev.cadm.socket)
                    (:gs :trivial-gray-streams)))

(in-package tls)

(defclass tls-context ()
  ((%context :initarg :context :initform (error "context required"))))

(defclass tls-session ()
  ((%session :initarg :session :initform (error "session required"))))

(defclass tls-stream (gs:fundamental-binary-input-stream
                      gs:fundamental-binary-output-stream)
  ((stream :initarg :stream :initform (error "stream required"))
   (in-buffer :initarg :in-buffer :initform (error "in-buffer required"))
   (out-buffer :initarg :out-buffer :initform (error "out-buffer required"))))

(defun make-tls-context ()
  (make-instance
   'tls-context
   :context
   #+abcl
   (let ((ssl-context (java:jstatic "getInstance"
                                     "javax.net.ssl.SSLContext"
                                     "TLSv1.3")))
     (java:jcall "init" ssl-context java:+null+ java:+null+ java:+null+)
     ssl-context)))

(defun make-tls-session (context)
  (make-instance
   'tls-session
   :session
   (with-slots (%context) context
     #+abcl
     (let ((ssl-engine (java:jcall "createSSLEngine" %context)))
       (java:jcall "setUseClientMode" ssl-engine t)
       ssl-engine))))

(defun make-tls-stream (session stream)
  (make-instance
   'tls-stream
   :stream
   #+abcl
   (let* ((ssl-engine %session)
          (ssl-session (java:jcall "getSession" ssl-engine))
          (app-buffer-size (java:jcall "getApplicationBufferSize" ssl-session))
          (net-buffer-size (java:jcall "getPacketBufferSize" ssl-session))
          (app-buffer (java:jstatic "allocate"
                                    "java.nio.ByteBuffer"
                                    (+ 50 app-buffer-size)))
          (net-buffer (java:jstatic "allocateDirect"
                                    "java.nio.ByteBuffer"
                                    net-buffer-size)))
     (pairlis '(ssl-engine app-buffer net-buffer)
              (list ssl-engine app-buffer net-buffer)))))

(defun jbuffer->byte-array (jbuffer)
  (loop with position = (java:jcall "position" jbuffer)
        with byte-array = (make-array position :element-type '(unsigned-byte 8))
        for i below position
        do (setf (aref byte-array i) (java:jcall "get" jbuffer i))
        finally (return byte-array)))

(defparameter +need-task+
  (java:jfield "javax.net.ssl.SSLEngineResult$HandshakeStatus" "NEED_TASK"))

(defun init-test ()
  (defparameter context (make-tls-context))
  (defparameter session (make-tls-session context))
  (defparameter ssl-engine (slot-value session '%session))
  (defparameter ssl-session (java:jcall "getSession" ssl-engine))
  (defparameter app-buffer-size (java:jcall "getApplicationBufferSize" ssl-session))
  (defparameter packet-buffer-size (java:jcall "getPacketBufferSize" ssl-session))
  (defparameter packet-buffer (java:jstatic "allocateDirect" "java.nio.ByteBuffer" packet-buffer-size))
  (defparameter app-buffer (java:jstatic "allocate" "java.nio.ByteBuffer" app-buffer-size))
  (defparameter socket (dev.cadm.socket:make-socket "localhost" 5555))
  (defparameter input-stream (dev.cadm.socket::make-socket-input-stream socket))
  (defparameter output-stream (dev.cadm.socket::make-socket-output-stream socket)))

;; ncat --listen --ssl 5555
(defun test ()
  (write "hello world" app-buffer packet-buffer input-stream output-stream ssl-engine))

(defmacro while (test &body body)
  `(loop while ,test do (progn ,@body)))

(defmacro while-let ((var form) &body body)
  `(loop for ,var = ,form while ,var do (progn ,@body)))

(defun engine-closed-p (engine)
  (and (java:jcall "isOutboundDone" engine)
       (java:jcall "isInboundDone" engine)))

(defun write (msg app-buffer packet-buffer input-stream output-stream engine)
  (let* ((out (a:line-up-last
                 (babel:string-to-octets msg)
                 (java:jnew-array-from-array "byte")
                 (java:jstatic "wrap" "java.nio.ByteBuffer")))
           (result (java:jcall "wrap" engine out packet-buffer))
           (status (java:jcall "getHandshakeStatus" result))
           (need-task-p (java:jequal status +need-task+)))
      (format t "status: ~A~%" (java:jcall "toString" status))
      (format t "writing first msg~%")
      ;; TODO check
      (write-sequence (jbuffer->byte-array packet-buffer) output-stream)
      (when need-task-p
        (loop for runnable = (java:jcall "getDelegatedTask" engine)
              while runnable
              do (java:jcall "run" runnable)))
      (format t "reading response~%")
      (sleep 5)
      (let* ((in (a:line-up-last
                  (a:read-stream-content-into-byte-vector input-stream)
                  (java:jnew-array-from-array "byte")
                  (java:jstatic "wrap" "java.nio.ByteBuffer")))
             (result (java:jcall "unwrap" engine in app-buffer))
             (status (java:jcall "getHandshakeStatus" result))
             (need-task-p (java:jequal status +need-task+)))
        (format t "status: ~A~%" (java:jcall "toString" status))
        (when need-task-p
          (while-let (runnable (java:jcall "getDelegatedTask" engine))
            (sleep 5)
            (format t "runnable run~%")
            (java:jcall "run" runnable))))))

;; write = wrap
;; read = unwrap
;;
;; Read the spec and try again


;; tls-stream
;;   - in
;;   - out
;;
;; tls-stream has internal buffers
;; on write, handle wrap() and statuses
;; on read, handle unwrap() and statuses
;;
;; Write = wrap + write
;; NEED_UNWRAP = read + unwrap
;; NEED_TASK = runnable run



