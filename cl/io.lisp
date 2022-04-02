
(defun cp-r (src out &key (collectp (constantly t))
                          (recursep (constantly t))
                          (copy-file-p (constantly t))
                          (verbose nil))
  "Copy recursively directory SRC to OUT"
  (let ((out (uiop:ensure-directory-pathname out))
        (src (uiop:ensure-directory-pathname src)))
    (uiop:collect-sub*directories src collectp recursep
      (lambda (dir)
        (let* ((prefix (length (pathname-directory out)))
               (basedir (list* :relative (nthcdr prefix (pathname-directory dir))))
               (p (make-pathname :defaults dir :directory basedir) )
               (to (merge-pathnames p out)))
          (when verbose
            (format t "~a -> ~a~%" dir to))
          (ensure-directories-exist to)
          (dolist (f (uiop:directory-files dir))
            (if (funcall copy-file-p f)
                (let* ((fname (make-pathname :name (pathname-name f)
                                             :type (pathname-type f)))
                       (fto (merge-pathnames fname to)))
                  (when verbose
                    (format t "~a -> ~a~%" f fto))
                  (uiop:copy-file f fto))
                (when verbose
                  (format t "skip ~a~%" f)))))))))
