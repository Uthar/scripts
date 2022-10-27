(defpackage kaspi.pool
  (:use :cl))

(in-package kaspi.pool)

;; Promises

(defclass promise ()
  ((value :initform nil)
   (resolvedp :initform nil)
   (lock :initform (sb-thread:make-mutex))
   (cv :initform (sb-thread:make-waitqueue))))

(defmethod deref ((promise promise))
  (sb-thread:with-mutex ((slot-value promise 'lock))
    (or (slot-value promise 'resolvedp)
        (sb-thread:condition-wait (slot-value promise 'cv)
                                  (slot-value promise 'lock)))
    (values (slot-value promise 'value)
            (slot-value promise 'resolvedp))))

(defmethod resolve ((promise promise) value)
  (sb-thread:with-mutex ((slot-value promise 'lock))
    (when (slot-value promise 'resolvedp)
      (error "Promise is already resolved."))
    (setf (slot-value promise 'value) value
          (slot-value promise 'resolvedp) t)
    (sb-thread:condition-notify (slot-value promise 'cv))))

(require 'sb-concurrency)

(defun make-worker (pool)
  (sb-thread:make-thread
   (lambda ()
     (loop
       (ignore-errors
        (funcall (sb-concurrency:receive-message (slot-value pool 'queue))))))
   :name "Pool worker"))

(defclass thread-pool ()
  ((threads :initform (vector))
   (queue :initform (sb-concurrency:make-mailbox))
   (shutdownp :initform nil))
  (:default-initargs
   :size 1))

(defmethod initialize-instance :after ((pool thread-pool) &key size)
  (setf (slot-value pool 'threads) (make-array size))
  (dotimes (n size)
    (setf (svref (slot-value pool 'threads) n) (make-worker pool))))

(defmethod shutdown ((pool thread-pool))
  (loop for thread across (slot-value pool 'threads)
        do (sb-thread:terminate-thread thread))
  (sb-ext:atomic-update (slot-value pool 'shutdownp) (constantly t)))

(defmethod submit ((pool thread-pool) (job function))
  (when (slot-value pool 'shutdownp)
    (error "Pool has shut down."))
  (let ((promise (make-instance 'promise))
        (job2 (lambda ()
                (resolve promise (funcall job2)))))
    (sb-concurrency:send-message (slot-value pool 'queue) job2)
    promise))

;; (defparameter pool (make-instance 'thread-pool :size 5))

;; (defparameter promise (submit pool (lambda () (sleep 5) (random 100))))

;; (deref promise)

;; (shutdown pool)
