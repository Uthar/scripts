(defpackage concurrent
  (:use
   :common-lisp)
  (:shadow
   :hash-table
   :gethash)
  (:export
   :hash-table
   :gethash))

(in-package concurrent)

(defclass concurrent-hash-table ()
  ((hash-table :initform (cl:make-hash-table :test 'equal))
   (lock :initform (sb-thread:make-mutex))))

(defgeneric gethash (key hash-table))

(defmethod gethash (key (hash-table concurrent-hash-table))
  (with-slots (hash-table lock)
      hash-table
    (sb-thread:with-mutex (lock)
      (cl:gethash key hash-table))))

(defmethod gethash (key (hash-table cl:hash-table))
  (cl:gethash key hash-table))

(defmethod (setf gethash) (value key (hash-table concurrent-hash-table))
  (with-slots (hash-table lock)
      hash-table
    (sb-thread:with-mutex (lock)
      (setf (cl:gethash key hash-table) value))))

(defmethod (setf gethash) (value key (hash-table cl:hash-table))
  (setf (cl:gethash key hash-table) value))
