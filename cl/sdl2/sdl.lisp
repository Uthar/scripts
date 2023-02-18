(defpackage sdl2/sdl
  (:use :cl)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl2/sdl)

(c:defbitfield init-flags
  (:timer          #0x00000001)
  (:audio          #0x00000010)
  (:video          #0x00000020)
  (:joystick       #0x00000200)
  (:haptic         #0x00001000)
  (:gamecontroller #0x00002000)
  (:events         #0x00004000)
  (:sensor         #0x00008000)
  #|
  (logior
   #0x00000001
   #0x00000010
   #0x00000020
   #0x00000200
   #0x00001000
   #0x00002000
   #0x00004000
   #0x00008000)
  |#
  (:everything 62001))

(c:defcfun ("SDL_Init" init) :int
  (flags init-flags))

(c:defcfun ("SDL_QuitSubSystem" quit-sub-system) :void
  (flags init-flags))

(c:defcfun ("SDL_WasInit" was-init) :uint32
  (flags init-flags))

(c:defcfun ("SDL_Quit" quit) :void)


