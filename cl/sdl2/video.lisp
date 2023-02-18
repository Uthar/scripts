(defpackage sdl2/video
  (:use :cl)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl2/video)

(c:defbitfield window-flags
  (:fullscreen  #0x00000001)
  (:opengl      #0x00000002)
  (:shown       #0x00000004)
  (:hidden      #0x00000008)
  (:borderless  #0x00000010)
  (:resizable   #0x00000020)
  (:minimized   #0x00000040)
  (:maximized   #0x00000080)
  (:mouse-grabbed  #0x00000100)
  (:input-focus    #0x00000200)
  (:mouse-focus    #0x00000400)
  (:fullscreen-desktop #0x00001001)
  (:foreign        #0x00000800)
  (:allow-highdpi  #0x00002000)
  (:mouse-capture  #0x00004000)
  (:always-on-top  #0x00008000)
  (:skip-taskbar   #0x00010000)
  (:utility        #0x00020000)
  (:tooltip        #0x00040000)
  (:popup-menu     #0x00080000)
  (:keyboard-grabbed  #0x00100000)
  (:vulkan  #0x10000000)
  (:metal   #0x20000000)
)
    
(c:defcfun ("SDL_GetNumVideoDisplays" get-num-video-displays) :int)

(c:defcfun ("SDL_GetDisplayName" get-display-name) :string
  (display-index :int))

(c:defcfun ("SDL_CreateWindow" create-window) :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags window-flags))

(c:defcfun ("SDL_DestroyWindow" destroy-window) :void
  (window :pointer))

(c:defcfun ("SDL_GL_CreateContext" gl-create-context) :pointer
  (window :pointer))

(c:defcfun ("SDL_GL_SwapWindow" gl-swap-window) :void
  (window :pointer))
