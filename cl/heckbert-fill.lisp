
(declaim (optimize speed))

(declaim (inline pixel-read))
(declaim (ftype (function ((vector (unsigned-byte 8))
                           (signed-byte 16)
                           (signed-byte 16)
                           (signed-byte 16)
                           (signed-byte 16))
                          (unsigned-byte 8))
                pixel-read))

(defun pixel-read (a w h x y)
  (if (and (< -1 x w)
           (< -1 y h))
      (aref a (+ (* y w) x))
      0))

(declaim (inline pixel-write))
(declaim (ftype (function ((vector (unsigned-byte 8))
                           (signed-byte 16)
                           (signed-byte 16)
                           (signed-byte 16)
                           (signed-byte 16)
                           (unsigned-byte 8))
                          (unsigned-byte 8))
                pixel-write))

(defun pixel-write (a w h x y nv)
  (when (and (< -1 x w)
             (< -1 y h))
    (setf (aref a (+ (* y w) x)) nv)))

(defun seed-fill (a w h x y nv)
  "Reference: see Paul Heckbert's stack-based seed fill algorithm in
'Graphic Gems', ed. Andrew Glassner, Academic Press, 1990.
The algorithm description is given on pp. 275-277; working C code is
on pp. 721-722."
  (declare (type fixnum x y))
  (let* ((l nil)
         (stack (list
                 (list y x x 1)
                 (list (1+ y) x x -1)))
         (ov (pixel-read a w h x y)))
    (when (= ov nv)
      (return-from seed-fill))
    (do () ((null (first stack)))
      (destructuring-bind (y x1 x2 dy) (pop stack)
        (declare (type fixnum y x1 x2)
                 (type (integer -1 1) dy))
        (do ((_ (setf x x1) (decf x)))
            ((not (= (pixel-read a w h x y) ov)))
          (pixel-write a w h x y nv))
        (tagbody
           (when (>= x x1)
             (go :skip))
           (setf l (1+ x))
           (when (< l x1)
             (push (list (- y dy) l (1- x1) (- dy)) stack))
           (setf x (1+ x1))
         :loop
           (do ((_ nil (incf x)))
               ((not (= (pixel-read a w h x y) ov)))
             (pixel-write a w h x y nv))
           (push (list (+ y dy) l (1- x) dy) stack)
           (when (> x (1+ x2))
             (push (list (- y dy) (1+ x2) (1- x) (- dy)) stack))
         :skip
           (do ((_ (incf x) (incf x)))
               ((or (> x x2) (= (pixel-read a w h x y) ov))))
           (setf l x)
           (when (<= x x2)
             (go :loop)))))))

#+(or)
(progn

  (defparameter image
    (let ((image (make-array '(600 1000))))
      (with-open-file (f "~/Documents/Untitled.data"
                         :element-type '(unsigned-byte 8))
        (dotimes (y 600)
          (dotimes (x 1000)
            (setf (aref image y x) (* 100 (read-byte f))))))
      image))

  (funcall (seed-fill-for image) 1 1 100)
  (funcall (seed-fill-for image) 40 200 100)
  (funcall (seed-fill-for image) 220 175 100)
  (funcall (seed-fill-for image) 550 110 100)
  (funcall (seed-fill-for image) 200 100 100)

  (with-open-file (f "~/out.data"
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-exists :supersede)
    (let ((seq (make-array (* 1000 600) :displaced-to image)))
      (write-sequence seq f))
    nil)

  )
  
#+(or)
(time 
(progn

  (defparameter screen
    (make-array (* 8000 8000)
                :element-type '(unsigned-byte 8)
                :initial-element 255))

  (time (seed-fill screen 8000 8000 30 30 127))

  (dotimes (i 16000)
    (dotimes (j 16000)
      (setf (aref screen (+ (* 16000 i) j)) (random 255))))

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
    (time (funcall fill 5 2 77)))
    (funcall fill 3 2 77)
    (funcall fill 3 0 33)
    (funcall fill 5 2 11)
    (funcall fill 3 2 78)
    (funcall fill 3 0 34))
    )

  screen

  nil))
