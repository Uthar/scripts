(defpackage :arrows
  (:use :common-lisp)
  (:export
   :->
   :->>
   :as->
   :cond->
   :cond->>
   :some->
   :some->>))

(in-package :arrows)

;;;; Utils

(defun ensure-list (x)
  (if (consp x)
      x
      (list x)))

(defun insert-first (x list)
  `(,(first list) ,x ,@(rest list)))

(defun insert-last (x list)
  `(,@list ,x))

(defun take (n list)
  (loop repeat n for x in list collect x))

(defun partition (n list)
  (loop for x = list then (nthcdr n x)
        while x
        collect (take n x)))

(defun make-keyword (name)
  (intern (symbol-name name) (find-package 'keyword)))

;;;; Internal definitions

(defun arrow-bindings (var init arrow body)
  `(let* ((,var ,init)
          ,@(mapcar (lambda (form)
                      (arrow-binding arrow var form))
                    (arrow-body arrow body)))
     ,var))

(defgeneric arrow-body (arrow body))

(defgeneric arrow-binding (arrow var form))

(defmacro defarrow (name &key var body-fn binding-fn)
  `(progn
     (defmethod arrow-binding ((arrow (eql ,(make-keyword name))) var form)
       (funcall ,binding-fn var form))
     (defmethod arrow-body ((arrow (eql ,(make-keyword name))) body)
       (funcall ,(or body-fn '#'identity) body))
     (defmacro ,name (init ,@(when var `(,var)) &rest body)
       (arrow-bindings ,(or var '(gensym)) init ,(make-keyword name) body))))

;;;; Public definitions

(defarrow ->
  :binding-fn
  (lambda (var form)
    `(,var ,(insert-first var (ensure-list form)))))

(defarrow ->>
  :binding-fn
  (lambda (var form)
    `(,var ,(insert-last var (ensure-list form)))))

(defarrow as->
  :var var
  :binding-fn
  (lambda (var form)
    `(,var ,form)))

(defarrow some->
  :binding-fn
  (lambda (var form)
    `(,var (when ,var (-> ,var ,form)))))

(defarrow some->>
  :binding-fn
  (lambda (var form)
    `(,var (when ,var (->> ,var ,form)))))

(defarrow cond->
  :body-fn
  (lambda (body)
    (partition 2 body))
  :binding-fn
  (lambda (var form)
    (destructuring-bind (test op) form
      `(,var (if ,test (-> ,var ,op) ,var)))))

(defarrow cond->>
  :body-fn
  (lambda (body)
    (partition 2 body))
  :binding-fn
  (lambda (var form)
    (destructuring-bind (test op) form
      `(,var (if ,test (->> ,var ,op) ,var)))))
