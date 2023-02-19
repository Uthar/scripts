(defpackage sdl2/keyboard
  (:use :cl)
  (:import-from :sdl2/scancode :scancode)
  (:import-from :sdl2/keycode :keycode)
  (:local-nicknames
   (:c :cffi))
  (:export
   #:keysym))

(in-package sdl2/keyboard)

(c:defcstruct keysym
  (scancode scancode)
  (keycode keycode)
  (mod :uint16))
