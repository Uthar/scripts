(defpackage sdl2/dll
  (:use :cl)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl2/dll)

(c:define-foreign-library SDL2
  (:windows "SDL2.dll")
  (:darwin "libSDL2.dylib")
  (:unix "libSDL2.so"))

(c:use-foreign-library SDL2)

;; (c:load-foreign-library
;;  "/nix/store/n5gjdmcrkslmwvkbbfk0qszm1jp0clvm-SDL2-2.24.2/lib/libSDL2.so")
