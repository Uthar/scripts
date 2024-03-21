(defpackage nogc (:use :cl))
(in-package nogc)

(declaim (ftype (function (fixnum) fixnum) tight))

(defun tight (x)
  (let ((buf (make-array 4000 :element-type 'fixnum :initial-element 0)))
    (declare (dynamic-extent buf))
    (dotimes (n 4000)
      (setf (aref buf n) (* n x)))
    (reduce #'logxor buf)))

(loop repeat 100
      do (time (tight 10)))

(time (loop repeat 100000 do (tight 123)))

(defun print-gc-time ()
  (format t "GC took ~a seconds~%" (/ sb-ext:*gc-run-time*
                                      internal-time-units-per-second
                                      1.0))
  (setf sb-ext:*gc-run-time* 0))

(defun loggc ()
  (format t "GC~%"))

(rplaca sb-ext:*after-gc-hooks* #'print-gc-time)
