


(in-package :sb-c)

(eval-when (:compile-toplevel)
  (defknown (cl-user::%rsqrt)
      (single-float) (single-float 0.0)
      (foldable flushable movable)))

(in-package :sb-vm)

(eval-when (:compile-toplevel)
  (define-vop (cl-user::%rsqrt)
    (:args (x :scs (single-reg)))
    (:results (y :scs (single-reg)))
    (:translate cl-user::%rsqrt)
    (:policy :fast-safe)
    (:arg-types single-float)
    (:result-types single-float)
    (:save-p :compute-only)
    (:generator 1 (inst rsqrtss y x))))


(in-package :cl-user)

(define-compiler-macro / (&whole whole number &rest more-numbers)
  (if (and (numberp number)
           (= number 1)
           (listp (first more-numbers))
           (= 1 (length more-numbers))
           (eql (first (first more-numbers)) 'sqrt))
      `(%rsqrt ,(second (first more-numbers)))
      whole))

(declaim (inline rsqrt))
(defun rsqrt (x)
  (%rsqrt x))

(setf (compiler-macro-function '/) nil)

(disassemble
 (lambda (x)
   (declare (type (single-float 0.0) x))
   (/ 1.0 (sqrt x))))

(funcall
(lambda (x)
   (declare (type (single-float 0.0) x))
  (/ 1.0 (sqrt x)))
49.0)

(disassemble 'calc)

(defun calc (x)
  (declare (type (single-float 0.0) x))
  (/ 1.0 (sqrt x)))

(disassemble
 (lambda (x)
   (rsqrt x)))
