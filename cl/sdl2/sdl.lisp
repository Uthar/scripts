(defpackage sdl2/sdl
  (:use :cl)
  (:local-nicknames
   (:c :cffi))
  (:export
   #:init
   #:quit-sub-system
   #:was-init
   #:quit))

(in-package sdl2/sdl)

(c:defbitfield init-flags
  (:timer          #x00000001)
  (:audio          #x00000010)
  (:video          #x00000020)
  (:joystick       #x00000200)
  (:haptic         #x00001000)
  (:gamecontroller #x00002000)
  (:events         #x00004000)
  (:sensor         #x00008000)
  #|
  (logior
   #x00000001
   #x00000010
   #x00000020
   #x00000200
   #x00001000
   #x00002000
   #x00004000
   #x00008000)
  |#
  (:everything 62001))

(c:defcfun ("SDL_Init" init) :int
  (flags init-flags))

(c:defcfun ("SDL_QuitSubSystem" quit-sub-system) :void
  (flags init-flags))

(c:defcfun ("SDL_WasInit" was-init) :uint32
  (flags init-flags))

(c:defcfun ("SDL_Quit" quit) :void)


