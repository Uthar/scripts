(defclass thread-pool ()
  ((semaphore :initform nil)
   (queue :initform nil)
   (dispatcher :initform nil))
  (:default-initargs
   :size 1))

(defun launch-job (job sem)
  (sb-thread:wait-on-semaphore sem)
  (sb-thread:make-thread
   (lambda ()
     (unwind-protect
          (funcall job)
       (sb-thread:signal-semaphore sem)))
   :name "Thread pool worker"))

(defun make-dispatcher (pool)
  (sb-thread:make-thread
   (lambda ()
     (loop
       (let ((job (sb-concurrency:receive-message (slot-value pool 'queue))))
         (launch-job job (slot-value pool 'semaphore)))))
   :name "Thread pool dispatcher"))

(defmethod initialize-instance :after ((pool thread-pool) &key size)
  (setf (slot-value pool 'semaphore)  (sb-thread:make-semaphore :count size)
        (slot-value pool 'queue)      (sb-concurrency:make-mailbox)
        (slot-value pool 'dispatcher) (make-dispatcher pool)))

(defmethod submit ((pool thread-pool) (job function))
  (sb-concurrency:send-message (slot-value pool 'queue) job))
