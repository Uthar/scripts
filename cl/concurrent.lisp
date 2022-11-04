(defpackage concurrent
  (:use
   :common-lisp)
  (:shadow
   :gethash
   :clrhash)
  (:local-nicknames
   (:bt :bordeaux-threads-2))
  (:export
   :concurrent-hash-table
   :make-concurrent-hash-table
   :gethash
   :clrhash))

(in-package concurrent)

(defclass concurrent-hash-table ()
  ((hash-table :initform #-abcl
                         (cl:make-hash-table :test 'equal)
                         #+abcl
                         (java:jnew "java.util.concurrent.ConcurrentHashMap"))
   (lock :initform (bt:make-lock))))

(defun make-concurrent-hash-table ()
  (make-instance 'concurrent-hash-table))

(defgeneric gethash (key hash-table))

(defmethod gethash (key (hash-table concurrent-hash-table))
  (with-slots (hash-table lock)
      hash-table
    #-abcl (bt:with-lock-held (lock)
             (cl:gethash key hash-table))
    #+abcl (java:jcall "get" hash-table key)))

(defmethod gethash (key (hash-table cl:hash-table))
  (cl:gethash key hash-table))

(defmethod (setf gethash) (value key (hash-table concurrent-hash-table))
  (with-slots (hash-table lock)
      hash-table
    #-abcl (bt:with-lock-held (lock)
             (setf (cl:gethash key hash-table) value))
    #+abcl (java:jcall "put" hash-table key value)))

(defmethod (setf gethash) (value key (hash-table cl:hash-table))
  (setf (cl:gethash key hash-table) value))

(defgeneric clrhash (hash-table))

(defmethod clrhash ((table cl:hash-table))
  (cl:clrhash table))

(defmethod clrhash ((table concurrent-hash-table))
  (with-slots (hash-table lock)
      table
    #-abcl (bt:with-lock-held (lock)
             (cl:clrhash hash-table))
    #+abcl (java:jcall "clear" hash-table)))
