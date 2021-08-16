(require :sb-posix)

(defmacro with-fork (&body body)
  (let ((pid (gensym "pid")))
    `(let ((,pid (sb-posix:fork)))
       (case (signum ,pid)
         (-1
          (error "fork failed"))
         (0
          ,@body
          (sb-posix:_exit 0))
         (1
          (sb-alien:with-alien ((status int))
            (sb-posix:waitpid ,pid status)))))))
