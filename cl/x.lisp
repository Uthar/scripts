(cffi:load-foreign-library "/nix/store/9mg4w9gk1bl4xx90painmfx47vn9acpl-libX11-1.7.2/lib/libX11.so")

(cffi:defcfun "XOpenDisplay" :pointer
  (display-name :string))

(cffi:defcfun "XDefaultScreenOfDisplay" :pointer
  (display :pointer))

(cffi:defcfun "XRootWindowOfScreen" :uint
  (screen :pointer))

(cffi:defcfun "XCreateSimpleWindow" :uint
  (display :pointer)
  (parent :uint)
  (x :int)
  (y :int)
  (width :uint)
  (height :uint)
  (border-width :uint)
  (border :ulong)
  (background :ulong))

(cffi:defcfun "XClearWindow" :void
  (display :pointer)
  (window :uint))

(cffi:defcfun "XMapRaised" :void
  (display :pointer)
  (window :uint))

(cffi:defcfun "XNextEvent" :void
  (display :pointer)
  (event-return :pointer))

(cffi:defcfun "XDestroyWindow" :void
  (display :pointer)
  (window :uint))

(cffi:defcfun "XFree" :void
  (screen :pointer))

(cffi:defcfun "XCloseDisplay" :void
  (display :pointer))

(cffi:defcfun "XSelectInput" :void
  (display :pointer)
  (window :uint)
  (event-mask :long))

(defparameter |NoEventMask|   0)
(defparameter |KeyPressMask|   (ash 1 0))
(defparameter |KeyReleaseMask|   (ash 1 1))
(defparameter |ButtonPressMask|   (ash 1 2))
(defparameter |ButtonReleaseMask|  (ash 1 3))
(defparameter |EnterWindowMask|   (ash 1 4))
(defparameter |LeaveWindowMask|   (ash 1 5))
(defparameter |PointerMotionMask|  (ash 1 6))
(defparameter |PointerMotionHintMask|  (ash 1 7))
(defparameter |Button1MotionMask|  (ash 1 8))
(defparameter |Button2MotionMask|  (ash 1 9))
(defparameter |Button3MotionMask|  (ash 1 10))
(defparameter |Button4MotionMask|  (ash 1 11))
(defparameter |Button5MotionMask|  (ash 1 12))
(defparameter |ButtonMotionMask|  (ash 1 13))
(defparameter |KeymapStateMask|   (ash 1 14))
(defparameter |ExposureMask|   (ash 1 15))
(defparameter |VisibilityChangeMask|  (ash 1 16))
(defparameter |StructureNotifyMask|  (ash 1 17))
(defparameter |ResizeRedirectMask|  (ash 1 18))
(defparameter |SubstructureNotifyMask|  (ash 1 19))
(defparameter |SubstructureRedirectMask| (ash 1 20))
(defparameter |FocusChangeMask|   (ash 1 21))
(defparameter |PropertyChangeMask|  (ash 1 22))
(defparameter |ColormapChangeMask|  (ash 1 23))
(defparameter |OwnerGrabButtonMask|  (ash 1 24))

(defparameter |KeyPress|  2)
(defparameter |KeyRelease|  3)
(defparameter |ButtonPress|  4)
(defparameter |ButtonRelease|  5)
(defparameter |MotionNotify|  6)
(defparameter |EnterNotify|  7)
(defparameter |LeaveNotify|  8)
(defparameter |FocusIn|   9)
(defparameter |FocusOut|  10)
(defparameter |KeymapNotify|  11)
(defparameter |Expose|   12)
(defparameter |GraphicsExpose|  13)
(defparameter |NoExpose|  14)
(defparameter |VisibilityNotify| 15)
(defparameter |CreateNotify|  16)
(defparameter |DestroyNotify|  17)
(defparameter |UnmapNotify|  18)
(defparameter |MapNotify|  19)
(defparameter |MapRequest|  20)
(defparameter |ReparentNotify|  21)
(defparameter |ConfigureNotify|  22)
(defparameter |ConfigureRequest| 23)
(defparameter |GravityNotify|  24)
(defparameter |ResizeRequest|  25)
(defparameter |CirculateNotify|  26)
(defparameter |CirculateRequest| 27)
(defparameter |PropertyNotify|  28)
(defparameter |SelectionClear|  29)
(defparameter |SelectionRequest| 30)
(defparameter |SelectionNotify|  31)
(defparameter |ColormapNotify|  32)
(defparameter |ClientMessage|  33)
(defparameter |MappingNotify|  34)
(defparameter |GenericEvent|  35)
(defparameter |LASTEvent|  36)

(cffi:defcfun "XSetErrorHandler" :pointer
  (handler :pointer))

(cffi:defcallback my-error-handler :int
    ((display :pointer)
     (x-error-event :pointer))
  (format t "X error~%"))

(xseterrorhandler (cffi:callback my-error-handler))

;; Open the display
(defparameter display (xopendisplay ":0"))
(defparameter screen (xdefaultscreenofdisplay display))

;; Open the window
(defparameter window
  (xcreatesimplewindow display
                       (xrootwindowofscreen screen)
                       100 100
                       800 600
                       1 0 0))

(xselectinput display window (logior |KeyPressMask|
                                     |KeyReleaseMask|))

;; Show the window
(xclearwindow display window)
(xmapraised display window)

;; Enter message loop
(defparameter xev (cffi:foreign-alloc :pointer))
(xnextevent display xev)

;; Event type
(cffi:mem-ref xev :int)

;; Cleanup
(xdestroywindow display window)
(xfree screen)
(xclosedisplay display)

