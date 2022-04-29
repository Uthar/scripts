#!/usr/bin/env -S sbcl --script

(require :uiop)

(defun sh (fmt &rest args)
  (let ((cmd (apply #' format (list* nil fmt args))))
    (nth-value 2 (uiop:run-program cmd :ignore-error-status t
                                       :output t
                                       :error-output t))))

(dolist (dir (uiop:subdirectories "."))
  (let ((repo (car (last (pathname-directory dir)))))
    (format t "~a~%" repo)
    (unless (zerop (sh "gh repo view ~a &> /dev/null" repo))
      (sh "gh repo create ~a --private" repo))
    (sh "git -C ~a push --mirror git@github.com:uthar/~a" repo repo)))
