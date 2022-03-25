(defpackage arrows-test
  (:use :common-lisp :fiveam :arrows))

(in-package :arrows-test)

(test test--> ()
  (is (= (-> 3 /) 1/3))
  (is (= (-> 3 (/)) 1/3))
  (is (= (-> 3 (/ 2)) 3/2))
  (is (= (-> 3 (/ 2) /) 2/3)))

(test test-->> ()
  (is (= (->> 3 /) 1/3))
  (is (= (->> 3 (/)) 1/3))
  (is (= (->> 3 (/ 2)) 2/3))
  (is (= (->> 3 (/ 2) /) 3/2)))

(test test-some-> ()
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 9))
                    first
                    (* 9))))
  (is (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 first
                 (* 9))
         72))
  (is (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 second
                 (* 9))
         81))
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 8 9))
                    third
                    (* 9))))
  (is (null (some-> '(:a 1)
                    (getf :b)
                    1+))))

(test test-some->> ()
  (is (= (some->> '((:a . 3) (:b . 5))
                  (assoc :a)
                  cdr
                  1+)
         4))
  (is (null (some->> '((:a . 3) (:b . 5))
                     (assoc :c)
                     cdr
                     1+))))

(test test-cond-> ()
  (is (equal (labels ((strcat (&rest things)
                        (apply #'concatenate 'string things))
                      (say (n)
                        (cond-> '()
                                (zerop (mod n 3)) (strcat "Fizz")
                                (zerop (mod n 5)) (strcat "Buzz")
                                t (or (princ-to-string n)))))
               (mapcar #'say '(9 10 11 12 13 14 15)))
             '("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"))))

(test test-cond->> ()
  (is (equal (labels ((concat (xs) (apply #'concatenate 'string xs))
                      (say (n)
                        (->
                         (cond->> '()
                                  (zerop (mod n 3)) (cons "Fizz")
                                  (zerop (mod n 5)) (cons "Buzz"))
                         (or (list (princ-to-string n)))
                         reverse
                         concat)))
               (mapcar #'say '(9 10 11 12 13 14 15)))
             '("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"))))

(test test-as-> ()
  (is (= (as-> 3 $
               (* 5 $)
               (/ $ 7))
         15/7))
  (is (= (as-> 0 n
               (1+ n)
               (1+ n))
         2)))
