
(cffi:load-foreign-library "/nix/store/6535q25w085jn2lk23v651jhl6iln56s-gnutls-3.7.8/lib/libgnutls.so")

(cffi:defcfun "gnutls_global_init" :int)

(cffi:defcfun "gnutls_init" :int
  (session :pointer)
  (flags :unsigned-int))

(cffi:defcfun "gnutls_record_get_direction" :int
  (session :pointer))

(cffi:defcfun "gnutls_session_get_flags" :int
  (session :pointer))

(cffi:defcfun "gnutls_set_default_priority" :int
  (session :pointer))

(cffi:defcfun "gnutls_certificate_allocate_credentials" :int
  (res :pointer))

(cffi:defcfun "gnutls_certificate_set_x509_system_trust" :int
  (cred :pointer))

(cffi:defcfun "gnutls_credentials_set" :int
  (session :pointer)
  (type :int)
  (cred :pointer))

(cffi:defcfun "gnutls_check_version" :int
  (version :string))

(cffi:defcfun "gnutls_transport_set_ptr" :void
  (session :pointer)
  (ptr :pointer))
(cffi:defcfun "gnutls_transport_set_push_function" :void
  (session :pointer)
  (func :pointer))
(cffi:defcfun "gnutls_transport_set_vec_push_function" :void
  (session :pointer)
  (func :pointer))
(cffi:defcfun "gnutls_transport_set_pull_function" :void
  (session :pointer)
  (func :pointer))
(cffi:defcfun "gnutls_transport_set_pull_timeout_function" :void
  (session :pointer)
  (func :pointer))

(cffi:defcfun "gnutls_handshake" :int
  (session :pointer))

(cffi:defcfun "gnutls_record_send" :size
  (session :pointer)
  (data :pointer)
  (size :size))

(cffi:defcfun "gnutls_record_recv" :size
  (session :pointer)
  (data :pointer)
  (size :size))

(cffi:defcfun "gnutls_error_is_fatal" :int
  (err :int))

(cffi:defcfun "gnutls_record_send" :int
  (session :pointer)
  (msg :string)
  (len :size))

(cffi:defcstruct giovec_t
  (iov_base :pointer)
  (iov_len :size))

(defparameter +gnutls-server+ 1)
(defparameter +gnutls-client+ (ash 1 1))
(defparameter +gnutls-datagram+ (ash 1 2))
(defparameter +gnutls-nonblock+ (ash 1 3))
(defparameter +gnutls-no-extensions+ (ash 1 4))
(defparameter +gnutls-no-replay-protection+ (ash 1 5))
(defparameter +gnutls-no-signal+ (ash 1 6))
(defparameter +gnutls-allow-id-change+ (ash 1 7))
(defparameter +gnutls-enable-false-start+ (ash 1 8))
(defparameter +gnutls-force-client-cert+ (ash 1 9))
(defparameter +gnutls-no-tickets+ (ash 1 10))
(defparameter +gnutls-key-share-top+ (ash 1 11))
(defparameter +gnutls-key-share-top2+ (ash 1 12))
(defparameter +gnutls-key-share-top3+ (ash 1 13))
(defparameter +gnutls-post-handshake-auth+ (ash 1 14))
(defparameter +gnutls-no-auto-rekey+ (ash 1 15))
(defparameter +gnutls-safe-padding-check+ (ash 1 16))
(defparameter +gnutls-enable-early-start+ (ash 1 17))
(defparameter +gnutls-enable-rawpk+ (ash 1 18))
(defparameter +gnutls-auto-reauth+ (ash 1 19))
(defparameter +gnutls-enable-early-data+ (ash 1 20))
(defparameter +gnutls-no-auto-send-ticket+ (ash 1 21))
(defparameter +gnutls-no-end-of-early-data+ (ash 1 22))
(defparameter +gnutls-no-tickets-tls12+ (ash 1 23))

(defparameter +gnutls-crd-certificate+ 1)

(defparameter +gnutls-e-again+ -28)

;;;;

(defparameter session (cffi:foreign-alloc :pointer))
(defparameter xcred (cffi:foreign-alloc :pointer))
(defvar socket (socket:make-socket "127.0.0.1" 5555))
(defvar *out* (socket:make-socket-output-stream socket))
(defvar *in* (socket:make-socket-input-stream socket))

(when (zerop (gnutls-check-version "3.6.5"))
  (warn "Unsupported GnuTLS version. Expect problems."))

(cffi:defcallback push-func :ssize ((transport-ptr :pointer)
                                    (buf :pointer)
                                    (size :size))
  (declare (ignorable transport-ptr))
  (loop for index below size
        collect (cffi:mem-ref buf :unsigned-char index) into bytes
        finally (write-sequence bytes *out*))
  size)

;; (cffi:defcallback vec-push-func :ssize ((transport-ptr :pointer)
;;                                         (iov :pointer)
;;                                         (iovcnt :int))
;;   (declare (ignorable transport-ptr))
;;   (print "vec-push-func called")
;;   (cffi:with-foreign-slots ((iov_base iov_len) iov (:struct giovec_t))
;;     ;; TODO(kasper): buffering
;;     (loop for index below iovcnt
;;           do (format t "write byte ~A~%" (cffi:mem-ref iov_base :unsigned-char index))          
;;           do (write-byte (cffi:mem-ref iov_base :unsigned-char index) *out*))
;;     ;; (format t "~% Done writing ~A bytes~%" iovcnt)
;;     iovcnt))

(cffi:defcallback pull-func :ssize ((transport-ptr :pointer)
                                    (buf :pointer)
                                    (size :size))
  (declare (ignorable transport-ptr))
  ;; (format t "pull-func called: ~A~%" transport-ptr)
  ;; TODO(kasper): buffering
  (loop for index below size
        do (setf (cffi:mem-ref buf :unsigned-char index)
                 (read-byte *in*)))
  (format t "~% Done reading ~A bytes~%" size)
  size)

;; (cffi:defcallback pull-timeout-func :int ((transport-ptr :pointer)
;;                                           (ms :uint))
;;   (declare (ignorable ms transport-ptr))
;;   (print "pull-timeout-func called")
;;   ;; TODO(kasper): implement properly
;;   32)

(defun init-gnutls ()

  (gnutls-global-init)
  
  (gnutls-certificate-allocate-credentials xcred)

  ;; Or:
  ;; gnutls_certificate_set_verify_flags(backend->cred,
  ;; GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT)
  ;;
  ;; gnutls_certificate_set_x509_trust_file(backend->cred,
  ;; SSL_CONN_CONFIG(CAfile),
  ;; GNUTLS_X509_FMT_PEM)
  
  (gnutls-certificate-set-x509-system-trust (cffi:mem-ref xcred :pointer))
  
  (gnutls-init session (logior +gnutls-client+))
  (gnutls-set-default-priority (cffi:mem-ref session :pointer))
  (gnutls-credentials-set (cffi:mem-ref session :pointer)
                          +gnutls-crd-certificate+
                          (cffi:mem-ref xcred :pointer))

  (gnutls-transport-set-ptr
   (cffi:mem-ref session :pointer)
   (cffi:null-pointer))
  
  (gnutls-transport-set-push-function
   (cffi:mem-ref session :pointer)
   (cffi:get-callback 'push-func))

  ;; (gnutls-transport-set-vec-push-function
  ;;  (cffi:mem-ref session :pointer)
  ;;  (cffi:get-callback 'vec-push-func))

  (gnutls-transport-set-pull-function
   (cffi:mem-ref session :pointer)
   (cffi:get-callback 'pull-func))

  ;; (gnutls-transport-set-pull-timeout-function
  ;;  (cffi:mem-ref session :pointer)
  ;;  (cffi:get-callback 'pull-timeout-func))

  ;; TODO(kasper): add session restore
  
  (loop for err = (gnutls-handshake (cffi:mem-ref session :pointer))
        while (and (< err 0) (zerop (gnutls-version-is-fatal err)))
        finally (return err))

  (gnutls-record-send (cffi:mem-ref session :pointer)
                      "hello world"
                      (length "hello world"))

;; GNUTLS_ENABLE_EARLY_START
;; gnutls_anty_replay_init
;; gnutls_anty_replay_enable
;; GNUTLS_CLIENT + TCP
;; GNUTLS_AUTO_REAUTH
;; GNUTLS_POST_HANDSHAKE_AUTH

;; (gnutls-record-get-direction gnutls-session)
;; (gnutls-session-get-flags gnutls-session)

;; gnutls_priority_set
;; gnutls_set_default_priority

;; gnutls_credentials_set

;; gnutls_certificate_set_x509_system_trust

;; gnutls_session_set_verify_cert


;; gnutls_transport_set_int
;; gnutls_transport_set_int2
;; gnutls_transport_set_ptr
;; gnutls_transport_set_ptr2
;; gnutls_transport_set_push_function
;; gnutls_transport_set_pull_function

;; Once a session has been initialized and a network connection has been set up,
;; TLS and DTLS protocols perform a handshake. The handshake is the actual key
;; exchange.

;; Once the handshake is complete and peer’s identity has been verified data can
;; be exchanged. The available functions resemble the POSIX recv and send
;; functions. It is suggested to use gnutls error is fatal to check whether the
;; error codes returned by these functions are fatal for the protocol or can be
;; ignored.

;; gnutls_record_send


;; Alerts must be handled

;; gnutls_alert_get
;; gnutls_alert_send


;; gnutls_bye
;; gnutls_deinit


;; Resume session
;; gnutls_session_get_data2
;; gnutls_session_set_data

;; Reauth
;; gnutls_reauth
