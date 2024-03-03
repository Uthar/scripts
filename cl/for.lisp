(defpackage for
  (:use :cl))

(in-package for)

(export 'for)

(defmacro for ((&rest bindings) &body body)
  `(loop ,@(mapcar (lambda (binding)
                     (destructuring-bind (sym thing) binding
                       ())))))


(defclass iterator () ())

(defgeneric next (iterator))

(defgeneric more-p (iterator))

(defmethod next :around ((iterator iterator))
  (if (more-p iterator)
      (call-next-method)
      +end+))

(defclass end () ())
(defparameter +end+ (make-instance 'end))

(defclass list-iterator (iterator)
  ((list :initarg :list :initform nil :type list)))

(defmethod next ((iterator list-iterator))
  (with-slots (list) iterator
    (prog1 (car list)
      (setf list (cdr list)))))

(defmethod more-p ((iterator list-iterator))
  (not (endp (slot-value iterator 'list))))

(defclass vector-iterator (iterator)
  ((vector :initarg :vector :initform #() :type vector)
   (index :initform 0 :type (integer 0))))

(defmethod next ((iterator vector-iterator))
  (with-slots (vector index) iterator
    (prog1 (aref vector index)
      (incf index))))

(defmethod more-p ((iterator vector-iterator))
  (with-slots (vector index) iterator
    (< index (length vector))))

(defclass hash-table-iterator (iterator)
  ((hash-table :initarg :hash-table :type hash-table)
   (hash-table-iterator :type (integer 0)))
  (:default-initargs
   :hash-table (make-hash-table)))

(defmethod initialize-instance :after ((iterator hash-table-iterator) &key hash-table)
  (print hash-table))

(make-instance 'hash-table-iterator)
  

(defmethod next ((iterator vector-iterator))
  (with-slots (vector index) iterator
    (prog1 (aref vector index)
      (incf index))))

(defmethod more-p ((iterator vector-iterator))
  (with-slots (vector index) iterator
    (< index (length vector))))

(let ((iter (make-instance 'list-iterator :list '(1 2 3))))
  (print (next iter))
  (print (next iter))
  (print (next iter))
  (print (next iter))
  (print (next iter)))

(let ((iter (make-instance 'vector-iterator :vector #(1 2 3))))
  (print (next iter))
  (print (next iter))
  (print (next iter))
  (print (next iter))
  (print (next iter)))


;; List
(loop :for n :in (list 1 2 3)
      :do (print n))

;; Want:
(for ((n (list 1 2 3)))
  (print n))

;; Vector
(loop :for n :across (vector 1 2 3)
      :do (print n))

;; Want
(for ((n (vector 1 2 3)))
  (print n))

;; List (Destructuring)
(loop :for (x y) :in (list '(1 2) '(3 4))
      :do (print x) (print y))

;; Want
(for (((x y) (list '(1 2) '(3 4))))
  (print x) (print y))

;; Vector (Destructuring)
(loop :for (x y) :across (vector '(1 2) '(3 4))
      :do (print x) (print y))

;; Want
(for (((x y) (vector '(1 2) '(3 4))))
  (print x) (print y))

;; Cons
(loop :for (x . y) :in (list '(1 . 2) '(3 . 4))
      :do (print x) (print y))

;; Want
(for (((x . y) (list '(1 . 2) '(3 . 4))))
  (print x) (print y))

;; Hash table
(let ((h (make-hash-table :test 'equal)))
  (setf (gethash "x" h) 10)
  (setf (gethash "y" h) 20)
  (loop :for k :being :the :hash-keys :of h :using (hash-value v)
        :do (format t "~A -> ~A~%" k v)))

;; Want
(let ((h (make-hash-table :test 'equal)))
  (setf (gethash "x" h) 10)
  (setf (gethash "y" h) 20)
  (for (((k v) h))
    (format t "~A -> ~A~%" k v)))
