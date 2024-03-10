(defpackage concurrent
  (:use :cl :sb-concurrency)
  (:export
   :make-count-down-latch
   :await
   :count-down))

(in-package concurrent)

(defstruct (latch (:constructor %make-latch)
                  (:copier nil))
  (gate (error "gate required") :type sb-concurrency:gate)
  (count (error "count required") :type (unsigned-byte 64))
  (name nil :type (or null simple-string)))

(defun make-count-down-latch (&key name count)
  (%make-latch
   :gate (sb-concurrency:make-gate :name name)
   :name name
   :count count))

(defun count-down (latch &optional (amount 1))
  (when (<= (sb-ext:atomic-decf (latch-count latch) amount)
            amount)
    (sb-concurrency:open-gate (latch-gate latch))))

(defun await (latch &optional timeout)
  (sb-concurrency:wait-on-gate (latch-gate latch) :timeout timeout))

#+(or)
(progn
  (defparameter *latch* (make-count-down-latch :count 11))
  (defun wait-for-it ()
    (await *latch*)
    (format t "Latch done: ~A~%" (sb-thread:thread-name sb-thread:*current-thread*)))
  (dotimes (n 10) (sb-thread:make-thread #'wait-for-it :name (format nil "Waiter #~a" n)))
  (count-down *latch* 3)
  )
