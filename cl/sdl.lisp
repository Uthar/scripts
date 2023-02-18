(defpackage sdl
  (:use :cl)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl)



(defun event-type (event)
  (let ((type (c:mem-ref event :uint32)))
    (c:foreign-enum-keyword 'event type)))


;; (defun make-keyboard-event (event)
;;   (let ((type (c:mem-ref event :uint32))
;;         (timestamp (c:mem-ref event :uint32 1))
;;         (window-id (c:mem-ref event :uint32 2))
;;         (state (c:mem-ref event :uint8 12))
;;         (repeat (c:mem-ref event :uint8 13))
;;         (scancode (c:mem-ref event 'scancode 4))
;;         (keycode (c:mem-ref event :uint32 5))
;;         (mod (c:mem-ref event :uint16 12)))
;;     (list type
;;           timestamp
;;           window-id
;;           state
;;           repeat
;;           scancode
;;           keycode
;;           mod)))

(c:mem-ref *event* '(:struct keyboard-event))

;; (c:mem-ref 
;; (c:foreign-slot-value
;;  *event*
;;  '(:struct keyboard-event)
;;  'keysym)
;; '(:struct keysym))
 
 

(init 0)
(defvar *window* (create-window "foo" 10 10 200 200 +window-opengl+))
(defvar *glctx* (gl-create-context *window*))
(defvar *event* (make-event))

(loop
  (sleep 0.2)
  (poll-event *event*)
  (when (eql :keydown (c:foreign-slot-value *event*
                                            '(:struct keyboard-event)
                                            'type))
    (format t "~A~%" (c:mem-ref *event* '(:struct keyboard-event)))))

(loop while (plusp (poll-event *event*)))

(make-keyboard-event *event*)

(destroy-window *window*)


