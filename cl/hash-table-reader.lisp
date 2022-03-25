
(defun hash-table-reader (stream char)
  (declare (ignore char))
  `(loop with map = (make-hash-table :test 'equal)
         with list = (list ,@(read-delimited-list #\} stream t))
         with keys = (loop for x in list by 'cddr collect x)
         with vals = (loop for x in (rest list) by 'cddr collect x)
         for key in keys
         for val in vals
         do (setf (gethash key map) val)
         finally (return map)))

(set-macro-character #\{ 'hash-table-reader)
(set-macro-character #\} (get-macro-character #\) nil))

(defmethod print-object ((object hash-table) stream)
  (let ((i 1)
        (size (hash-table-count object)))
    (format stream "{")
    (maphash (lambda (k v)
               (format stream "~s ~s" k v)
               (unless (= i size)
                 (format stream ", "))
               (incf i))
             object)
    (format stream "}")))

{:a 5 :b 6 :c 7 :d {:x 10 :y #(1 2 3)}}
