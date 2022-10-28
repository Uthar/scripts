
;; (setf (readtable-case *readtable*) :upcase)

(defmacro jimport (class)
  `(jimport-expand ',class))

(defun jimport-expand (class)
  `(progn
     (defparameter ,class (load-time-value (jclass ',class)))
     (defparameter ,(intern (subseq (symbol-name class)
                                    (1+ (position-if (lambda (c) (char= c #\.))
                                                     (symbol-name class)
                                                     :from-end t))))
       (load-time-value (jclass ',class)))
     (jdefuns ,class)
     (jimport-interfaces ,class)))

(defun expand-jimport-interfaces (class)
  (mapcar (lambda (i)
            (jimport-expand (intern (jcall "getName" i))))
          (jclass-all-interfaces (jclass class))))

(defmacro jimport-interfaces (class)
  `(eval-when (:load-toplevel :execute)
     ,@(expand-jimport-interfaces class)))
  

(defun params-for (method)
  (let ((arities (map 'list (lambda (param)
                              (intern (jcall "getName" param)))
                      (jcall "getParameters" method))))))

(defun all-arities-for (class)
  (let ((methods (jcall "getDeclaredMethods" (jclass class))))
    (loop with arities = (make-hash-table :test 'equal)
          for method across methods
          for name = (jcall "getName" method)
          for arity = (jcall "getParameters" method)
          do (setf (gethash name arities)
                   (cons arity (gethash name arities)))
          finally (return arities))))

(defun jdefun-arglist-for (arities)
  (let* ((min-arity (first (sort arities #'< :key #'length)))
         (max-arity (first (sort arities #'> :key #'length)))
         (offset (- (length max-arity) (length min-arity)))
         (arglist (append
                   (map 'list (lambda (parameter)
                                (intern (jcall "getName" parameter)))
                        min-arity)
                   '(&rest)
                   (list
                    (intern
                     (apply #'concatenate 'string
                            (map 'list (lambda (parameter)
                                         (concatenate 'string
                                                      (jcall "getName"
                                                             parameter)
                                                      "-"))
                                 (subseq max-arity offset))))))))
    arglist))

(defun expand-jdefuns (class)
  (eval-when (:load-toplevel :execute)
    (loop for method across (jcall "getDeclaredMethods" (jclass class))
          with all-arities = (all-arities-for class)
          with all-seen = (make-hash-table :test 'equal)
          for name = (jcall "getName" method)
          for seen = (gethash name all-seen)
          for arities = (gethash name all-arities)
          for arglist = (jdefun-arglist-for arities)
          when (not seen)
          collect (progn
                    (setf (gethash name all-seen) t)
                    `(defun ,(intern (concatenate 'string "." name))
                         (this ,@arglist)
                       (apply #'jcall ,method this ,@(remove '&rest arglist)))))))

(defmacro jdefuns (class)
  `(eval-when (:load-toplevel :execute)
     ,@(expand-jdefuns class)))

(defun init-java ()
  (jimport |java.util.concurrent.Executors|)
  (jimport |java.util.Map|)
  (jimport |java.util.HashMap|)
  (jimport |java.util.ArrayList|))

;; (defparameter hashmap (jnew |HashMap|))

;; (|.put| hashmap 3 10)

;; (jclass-interfaces (jclass '|java.util.ArrayList|))
;; (jss:jclass-all-interfaces (jclass '|java.util.ArrayList|))
;; (jclass-all-interfaces (jclass '|java.util.ArrayList|))

(defun jclass-all-interfaces (class)
  "Return a list of interfaces the class implements"
  (unless (java-object-p class)
    (setq class (jclass class)))
  (loop for aclass = class then (jcall "getSuperclass" aclass)
     while aclass
     append (coerce (jcall "getInterfaces" aclass) 'list)))
