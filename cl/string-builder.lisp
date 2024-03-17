(defpackage string-builder
  (:use :cl)
  (:shadow :append)
  (:export :make-string-builder :append))

(in-package string-builder)

(deftype u8 () `(unsigned-byte 8))
(deftype buffer () `(vector u8))

(declaim (ftype (function (buffer buffer) buffer) append))

(defun append (sb bytes)
  (let ((len1 (length sb)))
    (adjust-array sb (+ len1 (length bytes)))
    (replace sb bytes :start1 len1)))

(declaim (ftype (function () buffer) make-string-builder))

(defun make-string-builder ()
  (make-array 0 :element-type 'u8 :adjustable t))

#+nil
(progn
  (defparameter *sb* (make-string-builder))
  (length *sb*)
  (append *sb* (make-array 5 :element-type 'u8 :initial-contents #(1 2 3 4 5)))
  )
