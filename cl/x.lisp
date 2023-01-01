(in-package yrmadis)

(defun julianday ()
  (+ (/ (unixepoch) 86400.0d0) 2440587.5d0))

#+unix
(cffi:defcstruct timeval
  (sec :uint64)
  (usec :uint64))

#+unix
(defun unixepoch ()
  (cffi:with-foreign-object (tv '(:struct timeval))
    (cffi:foreign-funcall "gettimeofday"
                          (:pointer (:struct timeval)) tv
                          :pointer (cffi:null-pointer)
                          :int)
    (cffi:with-foreign-slots ((sec usec) tv (:struct timeval))
      (+ sec (/ usec 1000000.0d0)))))

#+win32
(defun unixepoch ()
  (error "Not implemented yet"))

#-(or win32 unix)
(defun unixepoch ()
  (error "Not implemented"))


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

(defparameter display (xopendisplay ":0"))
(defparameter screen (xdefaultscreenofdisplay display))
(defparameter root-window (xrootwindowofscreen screen))
(defparameter window (xcreatesimplewindow display root-window 100 100 100 100 0 0 0))
(xselectinput display window (logior |KeyPressMask|
                                     |StructureNotifyMask|
                                     |NoEventMask|))
(xclearwindow display window)
(xmapraised display window)
(defparameter xev (cffi:foreign-alloc :pointer))
(xnextevent display xev)
(xdestroywindow display window)
(xfree screen)
(xclosedisplay display)
