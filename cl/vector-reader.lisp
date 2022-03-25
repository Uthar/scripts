

(defun vector-reader (stream char)
  (declare (ignore char))
  `(vector ,@(read-delimited-list #\] stream t)))

(set-macro-character #\[ 'vector-reader)
(set-macro-character #\] (get-macro-character #\) nil))

(defmethod print-object ((object vector) stream)
  (let ((i 1)
        (size (length object)))
    (format stream "[")
    (format stream "~s" object)
    (format stream "]")))

#(1 2 3)

[1 2 3]

(find-class 'vector)

(vector 1 2 3)

(format t "[")
(format t "]")
