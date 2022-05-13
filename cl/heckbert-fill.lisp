
;; (declaim (optimize debug))

(defun seed-fill (x y inside write)
  "Reference: see Paul Heckbert's stack-based seed fill algorithm in
'Graphic Gems', ed. Andrew Glassner, Academic Press, 1990.
The algorithm description is given on pp. 275-277; working C code is
on pp. 721-722."
  (let* ((l nil)
         (stack (list
                 (list y x x -1)
                 (list (1+ y) x x 1))))
    (do () ((null (first stack)))
      (destructuring-bind (y x1 x2 dy) (pop stack)
        (do ((_ (setf x x1) (decf x)))
            ((not (funcall inside x y)))
          (funcall write x y))
        (tagbody
           (when (>= x x1)
             (go :skip))
           (setf l (1+ x))
           (when (< l x1)
             (push (list (- y dy) l (1- x1) (- dy)) stack))
           (setf x (1+ x1))
         :loop
           (do ((_ nil (incf x)))
               ((not (funcall inside x y)))
             (funcall write x y))
           (push (list (+ y dy) l (1- x) dy) stack)
           (when (> x (1+ x2))
             (push (list (- y dy) (1+ x2) (1- x) (- dy)) stack))
         :skip
           (do ((_ (incf x) (incf x)))
               ((or (> x x2) (funcall inside x y))))
           (setf l x)
           (when (<= x x2)
             (go :loop)))))))

(defun make-inside-function (screen x y &key (test #'eql))
  (let ((ov (aref screen y x)))
    (lambda (x y)
      (funcall test (ignore-errors (aref screen y x)) ov))))

(defun make-write-function (screen nv)
  (lambda (x y)
    (setf (aref screen y x) nv)))

(defun seed-fill-for (array2 &key (test #'eql))
  (check-type array2 (array * (* *)))
  (let ((x-max (1- (array-dimension array2 1)))
        (y-max (1- (array-dimension array2 0))))
    (lambda (x y nv)
      (assert (<= 0 x x-max) (x))
      (assert (<= 0 y y-max) (y))
      (unless (funcall test (aref array2 y x) nv)
        (let ((inside (make-inside-function array2 x y :test test))
              (write (make-write-function array2 nv)))
          (seed-fill x y inside write))))))
               
    
#+(or)
(progn

  (defparameter screen
    (make-array '(10 10)))

  (progn
    (setf (aref screen 3 3) 13)
    (setf (aref screen 3 4) 13)
    (setf (aref screen 3 5) 13)
    (setf (aref screen 3 6) 13)
    (setf (aref screen 2 3) 13)
    (setf (aref screen 2 6) 13)
    (setf (aref screen 1 3) 13)
    (setf (aref screen 1 4) 13)
    (setf (aref screen 1 5) 13)
    (setf (aref screen 1 6) 13)
    )

  (let ((fill (seed-fill-for screen)))
    (time (funcall fill 5 2 10))
    (time (funcall fill 3 2 77))
    (time (funcall fill 3 0 33))
    (time (funcall fill 5 2 11))
    (time (funcall fill 3 2 78))
    (time (funcall fill 3 0 34))
    )

  screen)
