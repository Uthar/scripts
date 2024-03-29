(defpackage for
  (:use :cl))

(in-package for)

;; TODO: Add an iterator that works on all (and SBCL's extensible) sequences

(export 'for)

(defmacro for ((&rest bindings) &body body)
  (let ((bindings (mapcar (lambda (b) (list* (gensym "ITER") b)) bindings)))
    `(handler-case
         (loop ,@(mapcan (lambda (binding)
                           (destructuring-bind (gsym var thing) binding
                             (declare (ignore var))
                             `(:with ,gsym := (iterator ,thing))))
                         bindings)
               ,@(mapcan (lambda (binding)
                           (destructuring-bind (gsym var thing) binding
                             (declare (ignore thing))
                             `(:for ,var := (next ,gsym))))
                         bindings)
               :do (progn ,@body))
       (end (e)
         (declare (ignore e))
         (values)))))

(defun iterator (thing)
  (etypecase thing
    (cons (make-instance 'list-iterator :list thing))
    (vector (make-instance 'vector-iterator :vector thing))
    (hash-table (make-instance 'hash-table-iterator :hash-table thing))))

(comment
  (defparameter things (make-hash-table :test 'equal))
  (setf (gethash "x" things) 42)
  (setf (gethash "y" things) 43)
  (setf (gethash "z" things) 44)

  (for ((x (list 1 2 3))
        (y (vector 4 5 6))
        ((k . v) things))
    (print (list x y k v)))

  ;; vs

  (loop :for x :in (list 1 2 3)
        :for y :across (vector 4 5 6)
        :for k :being :the :hash-keys :of things :using (hash-value v)
    :do (print (list x y k v)))
  )

(defclass iterator () ())

(defgeneric next (iterator))

(defgeneric more-p (iterator))

(defmethod next :around ((iterator iterator))
  (unless (more-p iterator)
    (error 'end))
  (call-next-method))

(define-condition end (error) ()
  (:report 
   (lambda (e stream)
     (declare (ignore e))
     (format stream "Iterator reached the end."))))

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

(defclass hash-table-iterator (wrapped-iterator)
  ((hash-table :initarg :hash-table :type hash-table))
  (:default-initargs
   :hash-table (make-hash-table)))

;; TODO think about how to make hash table iterator non consing

(defmethod initialize-instance :after
  ((iterator hash-table-iterator) &key hash-table)
  (setf (slot-value iterator 'wrapped)
        (make-instance 'funcall-iterator 
          :function
          (with-hash-table-iterator (ht-iterator hash-table)
            (lambda ()
              (multiple-value-bind (exists key value) (ht-iterator)
                (cons (cons key value) exists)))))))

(defclass wrapped-iterator ()
  ((wrapped :initarg :wrapped :type iterator))
  (:default-initargs
   :wrapped (make-instance 'list-iterator)))

(defmethod more-p ((iterator wrapped-iterator))
  (more-p (slot-value iterator 'wrapped)))

(defmethod next ((iterator wrapped-iterator))
  (next (slot-value iterator 'wrapped)))

(defclass funcall-iterator (iterator)
  ((function :initarg :function :type function)
   (last-value :type t))
  (:default-initargs
   :function (lambda () (cons nil nil))))

(defmethod next ((iterator funcall-iterator))
  (slot-value iterator 'last-value))

(defmethod more-p ((iterator funcall-iterator))
  (destructuring-bind (value . exists)
      (funcall (slot-value iterator 'function))
    (setf (slot-value iterator 'last-value) value)
    exists))

(comment
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

  (let* ((ht (make-hash-table :test 'equal))
         (_ (setf (gethash "x" ht) 42
                  (gethash "y" ht) 43
                  (gethash "z" ht) 44))
         ;; (_ (print (hash-table-count ht)))
         (iter (make-instance 'hash-table-iterator :hash-table ht)))
    (print (next iter))
    (print (next iter))
    (print (next iter))
    (print (next iter))
    (print (next iter))
    (print (next iter))))

(defmacro comment (&body body)
  (declare (ignore body))
  (values))

(comment
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
  )


;; Experimental stuff

(comment
  (defmacro %for2 ((&rest bindings) &body body)
    (let ((bindings (mapcar (lambda (b) (list* (gensym "ITER") (gensym "VAR") b)) bindings)))
      `(let ,(mapcar (lambda (binding)
                       (destructuring-bind (giter gvar bind thing) binding
                         (declare (ignore var))
                         `(,giter (iterator ,thing))))
              bindings)
         (handler-case
             (do ,(mapcar (lambda (binding)
                            (destructuring-bind (giter gvar bind thing) binding
                              (declare (ignore var))
                              `(,gvar (next ,giter) (next ,giter))))
                          bindings)
                 (nil (values))
               ,(reduce (lambda (r b)
                          (destructuring-bind (giter gvar bind thing) b
                            (cond
                              ((and (consp bind) (eq 'values (car bind)))
                               `(multiple-value-bind ,(cdr bind) ,gvar ,r))
                              ((consp bind)
                               `(destructuring-bind ,bind ,gvar ,r))
                              ((symbolp bind)
                               `(let ((,bind ,gvar)) ,r))
                              (t (error "Unsupported binding")))))
                        bindings
                        :initial-value `(progn ,@body)))
           (end (e)
             (declare (ignore e))
             (values))))))
  )
