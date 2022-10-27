;; (defclass thread-pool ()
;;   ((semaphore :initform nil)
;;    (queue :initform nil)
;;    (dispatcher :initform nil))
;;   (:default-initargs
;;    :size 1))

;; (defun launch-job (job sem)
;;   (sb-thread:wait-on-semaphore sem)
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (unwind-protect
;;           (funcall job)
;;        (sb-thread:signal-semaphore sem)))
;;    :name "Thread pool worker"))

;; (defun make-dispatcher (pool)
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (loop
;;        (let ((job (sb-concurrency:receive-message (slot-value pool 'queue))))
;;          (launch-job job (slot-value pool 'semaphore)))))
;;    :name "Thread pool dispatcher"))

;; (defmethod initialize-instance :after ((pool thread-pool) &key size)
;;   (setf (slot-value pool 'semaphore)  (sb-thread:make-semaphore :count size)
;;         (slot-value pool 'queue)      (sb-concurrency:make-mailbox)
;;         (slot-value pool 'dispatcher) (make-dispatcher pool)))

;; (defmethod submit ((pool thread-pool) (job function))
;;   (sb-concurrency:send-message (slot-value pool 'queue) job))

;; (submit pool (lambda () (sleep 1) (print 42)))

;; TODO dont launch new threads, run X threads with a recv loop instead

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
  (setf (slot-value pool 'shutdownp) t))

(defmethod submit ((pool thread-pool) (job function))
  (when (slot-value pool 'shutdownp)
    (error "Pool has shut down."))
  (sb-concurrency:send-message (slot-value pool 'queue) job))

(defparameter pool (make-instance 'thread-pool :size 5))

(submit pool (lambda () (sleep 1) (print (slot-value pool 'queue))))

(shutdown pool)
