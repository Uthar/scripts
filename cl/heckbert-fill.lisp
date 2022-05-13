
;; (declaim (optimize debug))

(defun heckbert-fill (x y inside write)
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

#+(or)
(progn

  (defparameter screen
    (make-array '(10 15)))

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

  (heckbert-fill 5 2
                 (lambda (x y)
                   (eql (ignore-errors (aref screen y x)) 0))
                 (lambda (x y)
                   (setf (aref screen y x) 10)))
  (heckbert-fill 3 2
                 (lambda (x y)
                   (eql (ignore-errors (aref screen y x)) 13))
                 (lambda (x y)
                   (setf (aref screen y x) 77)))
  (heckbert-fill 3 0
                 (lambda (x y)
                   (eql (ignore-errors (aref screen y x)) 0))
                 (lambda (x y)
                   (setf (aref screen y x) 33)))
  ;; (heckbert-fill screen 5 2 11)
  ;; (heckbert-fill screen 3 2 87)
  ;; (heckbert-fill screen 3 0 44)

  screen)
