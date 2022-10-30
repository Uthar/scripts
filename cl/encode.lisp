(defpackage encode
  (:use :common-lisp)
  (:export
   :octets->string
   :string->octets))

(in-package encode)

(defun octets->string (array)
  #+abcl
  (java:jcall
   "toString"
   (java:jnew "java.lang.String"
              (java:jnew-array-from-array "byte" array) "UTF-8")))

#+abcl
(defun jarray->byte-array (jarray)
  (declare (optimize speed))
  (loop
    with length = (java:jarray-length jarray)
    with array = (make-array length :element-type '(unsigned-byte 8))
    for index below length
    do (setf (aref array index) (java:jarray-ref jarray index))
    finally (return array)))

(defun string->octets (string)
  (jarray->byte-array (java:jcall "getBytes" string)))
