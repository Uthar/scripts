(sb-ext:add-package-local-nickname 'c 'sb-alien)

(c:load-shared-object
 "/nix/store/08qm8vakrgz6m0wk2lm83lh9rrkz3gqw-libX11-1.8.1/lib/libX11.so")

(c:define-alien-routine "XInitThreads" void)

(c:define-alien-routine "XOpenDisplay" (* t)
  (arg c-string))

(c:define-alien-routine "XDefaultScreenOfDisplay" (* t)
  (display (* t)))

(c:define-alien-routine "XCreateSimpleWindow" unsigned-int
  (display (* t))
  (window unsigned-int)
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (border-width unsigned-int)
  (border unsigned-long)
  (background unsigned-long))

(c:define-alien-routine "XRootWindow" unsigned-int
  (display (* t))
  (screen-number int))

(c:define-alien-routine "XSelectInput" void
  (display (* t))
  (window unsigned-int)
  (mask long))

(c:define-alien-routine "XMapRaised" void
  (display (* t))
  (window unsigned-int))

(c:define-alien-routine "XNextEvent" void
  (display (* t))
  (event (* t)))


(XInitThreads)

(defparameter d (XOpenDisplay ":0"))
(defparameter s (XDefaultScreenOfDisplay d))
(defparameter w (XCreateSimpleWindow d (XRootWindow d 0)
                                     0 0
                                     800 600
                                     0 0 0))

(defparameter KeyPressMask (ash 1 0))
(defparameter KeyReleaseMask (ash 1 1))

(XSelectInput d w (logior KeyPressMask KeyReleaseMask))

(XMapRaised d w)

(defparameter e (c:make-alien long 26))

(dotimes (_ 100)
  (XNextEvent d e)
  (format t "~a~%" (logand #xffff (c:deref e 0))))
