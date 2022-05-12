
;; (declaim (optimize debug))

(defun heckbert-fill (buf x y nv)
  "Set the pixel at (x,y) and all of its 4-connected neighbors
with the same pixel value to the new pixel value nv.
A 4-connected neighbor is a pixel above, below, left, or right of a pixel.

Reference: see Paul Heckbert's stack-based seed fill algorithm in
'Graphic Gems', ed. Andrew Glassner, Academic Press, 1990.
The algorithm description is given on pp. 275-277; working C code is
on pp. 721-722."
  (let* ((l nil)
         (y-max (array-dimension buf 0))
         (x-max (array-dimension buf 1))
         (stack (list
                 (list (1+ y) x x -1)
                 (list y x x 1)))
         (ov (aref buf y x))
         (push-segment (lambda (y xl xr dy)
                         (unless (or (minusp (+ y dy))
                                     (>= (+ y dy) y-max))
                           (push (list y xl xr dy) stack)))))
    (when (eql ov nv)
      (return-from heckbert-fill))
    (do () ((null (first stack)))
      (destructuring-bind (y x1 x2 dy) (pop stack)
        (incf y dy)
        (do ((_ (setf x x1) (decf x)))
            ((or (minusp x) (not (eql (aref buf y x) ov))))
          (setf (aref buf y x) nv))
        (tagbody
           (when (>= x x1)
             (go :skip))
           (setf l (1+ x))
           (when (< l x1)
             (funcall push-segment y l (1- x1) (- dy)))
           (setf x (1+ x1))
         :loop
           (do ((_ nil (incf x)))
               ((or (>= x x-max) (not (eql (aref buf y x) ov))))
             (setf (aref buf y x) nv))
           (funcall push-segment y l (1- x) dy)
           (when (> x (1+ x2))
             (funcall push-segment y (1+ x2) (1- x) (- dy)))
         :skip
           (do ((_ (incf x) (incf x)))
               ((or (> x x2) (>= x x-max) (eql (aref buf y x) ov))))
           (setf l x)
           (when (<= x x2)
             (go :loop)))))))

#+(or)
(progn

  (defparameter screen
    (make-array '(5 10)))

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

  (heckbert-fill screen 5 2 11)
  (heckbert-fill screen 3 2 12)
  (heckbert-fill screen 3 0 91)

  screen)
