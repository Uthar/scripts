(print (loop with err = nil
             while (null err)
             for x = (or (ignore-errors (read-line *standard-input*))
                         (setf err t))
             sum (or (ignore-errors (parse-integer x)) 0)))
