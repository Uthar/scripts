(defpackage sdl
  (:use :cl)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl)

(defparameter +WINDOW-FULLSCREEN+  #0x00000001)
(defparameter +WINDOW-OPENGL+      #0x00000002)
(defparameter +WINDOW-SHOWN+       #0x00000004)
(defparameter +WINDOW-HIDDEN+      #0x00000008)
(defparameter +WINDOW-BORDERLESS+  #0x00000010)
(defparameter +WINDOW-RESIZABLE+   #0x00000020)
(defparameter +WINDOW-MINIMIZED+   #0x00000040)
(defparameter +WINDOW-MAXIMIZED+   #0x00000080)
(defparameter +WINDOW-MOUSE-GRABBED+  #0x00000100)
(defparameter +WINDOW-INPUT-FOCUS+    #0x00000200)
(defparameter +WINDOW-MOUSE-FOCUS+    #0x00000400)
(defparameter +WINDOW-FULLSCREEN-DESKTOP+ (logior +WINDOW-FULLSCREEN+ #0x00001000))
(defparameter +WINDOW-FOREIGN+        #0x00000800)
(defparameter +WINDOW-ALLOW-HIGHDPI+  #0x00002000)
(defparameter +WINDOW-MOUSE-CAPTURE+  #0x00004000)
(defparameter +WINDOW-ALWAYS-ON-TOP+  #0x00008000)
(defparameter +WINDOW-SKIP-TASKBAR+   #0x00010000)
(defparameter +WINDOW-UTILITY+        #0x00020000)
(defparameter +WINDOW-TOOLTIP+        #0x00040000)
(defparameter +WINDOW-POPUP-MENU+     #0x00080000)
(defparameter +WINDOW-KEYBOARD-GRABBED+  #0x00100000)
(defparameter +WINDOW-VULKAN+  #0x10000000)
(defparameter +WINDOW-METAL+   #0x20000000)
(defparameter +WINDOW-INPUT-GRABBED+ +WINDOW-MOUSE-GRABBED+)

(c:defcenum event
  ;; application events
  (:quit #0x100)
  :app-terminating
  :app-lowmemory
  :app-willenterbackground 
  :app-willenterforeground 
  :app-didenterforeground 
  :localechanged

  ;; display events
  (:displayevent #0x150)

  ;; window events
  (:windowevent     #0x200)
  :syswmevent             

  ;; keyboard events
  (:keydown         #0x300)
  :keyup                  
  :textediting            
  :textinput              
  :keymapchanged          
  :textediting-ext       

  ;; mouse events
  (:mousemotion     #0x400)
  :mousebuttondown        
  :mousebuttonup          
  :mousewheel             

  ;; joystick events
  (:joyaxismotion   #0x600)
  :joyballmotion          
  :joyhatmotion           
  :joybuttondown          
  :joybuttonup            
  :joydeviceadded         
  :joydeviceremoved       
  :joybatteryupdated      

  ;; game controller events
  (:controlleraxismotion   #0x650)
  :controllerbuttondown          
  :controllerbuttonup            
  :controllerdeviceadded         
  :controllerdeviceremoved       
  :controllerdeviceremapped      
  :controllertouchpaddown        
  :controllertouchpadmotion      
  :controllertouchpadup          
  :controllersensorupdate        

  ;; touch events
  (:fingerdown       #0x70)
  :fingerup
  :fingermotion

  ;; gesture events
  (:dollargesture    #0x80)
  :dollarrecord
  :multigesture

  ;; clipboard events
  (:clipboardupdate  #0x900)

  ;; drag and drop events
  (:dropfile         #0x1000)
  :droptext                 
  :dropbegin                
  :dropcomplete             

  ;; audio hotplug events
  (:audiodeviceadded  #0x1100)
  :audiodeviceremoved        

  ;; sensor events
  (:sensorupdate  #0x1200)

  ;; render events
  (:render-targets-reset  #0x2000)
  :render-device-reset 

  ;; internal events
  (:pollsentinel  #0x7f00)

  (:userevent     #0x800)
  (:lastevent     #0xff))


(c:defcenum scancode
  (:unknown  0)
  
  (:a  4)
  (:b  5)
  (:c  6)
  (:d  7)
  (:e  8)
  (:f  9)
  (:g  10)
  (:h  11)
  (:i  12)
  (:j  13)
  (:k  14)
  (:l  15)
  (:m  16)
  (:n  17)
  (:o  18)
  (:p  19)
  (:q  20)
  (:r  21)
  (:s  22)
  (:t  23)
  (:u  24)
  (:v  25)
  (:w  26)
  (:x  27)
  (:y  28)
  (:z  29)

  (:1  30)
  (:2  31)
  (:3  32)
  (:4  33)
  (:5  34)
  (:6  35)
  (:7  36)
  (:8  37)
  (:9  38)
  (:0  39)

  (:return  40)
  (:escape  41)
  (:backspace  42)
  (:tab  43)
  (:space  44)

  (:minus  45)
  (:equals  46)
  (:leftbracket  47)
  (:rightbracket  48)
  (:backslash  49)
  (:nonushash  50) 
  (:semicolon  51)
  (:apostrophe  52)
  (:grave  53)

  (:comma  54)
  (:period  55)
  (:slash  56)

  (:capslock  57)

  (:f1  58)
  (:f2  59)
  (:f3  60)
  (:f4  61)
  (:f5  62)
  (:f6  63)
  (:f7  64)
  (:f8  65)
  (:f9  66)
  (:f10  67)
  (:f11  68)
  (:f12  69)

  (:printscreen  70)
  (:scrolllock  71)
  (:pause  72)
  (:insert  73)

  (:home  74)
  (:pageup  75)
  (:delete  76)
  (:end  77)
  (:pagedown  78)
  (:right  79)
  (:left  80)
  (:down  81)
  (:up  82)

  (:numlockclear  83)

  (:kp-divide  84)
  (:kp-multiply  85)
  (:kp-minus  86)
  (:kp-plus  87)
  (:kp-enter  88)
  (:kp-1  89)
  (:kp-2  90)
  (:kp-3  91)
  (:kp-4  92)
  (:kp-5  93)
  (:kp-6  94)
  (:kp-7  95)
  (:kp-8  96)
  (:kp-9  97)
  (:kp-0  98)
  (:kp-period  99)

  (:nonusbackslash  100)

  (:application  101)
  (:power  102)

  (:kp-equals  103)
  (:f13  104)
  (:f14  105)
  (:f15  106)
  (:f16  107)
  (:f17  108)
  (:f18  109)
  (:f19  110)
  (:f20  111)
  (:f21  112)
  (:f22  113)
  (:f23  114)
  (:f24  115)
  (:execute  116)
  (:help  117)
  (:menu  118)
  (:select  119)
  (:stop  120)
  (:again  121)
  (:undo  122)
  (:cut  123)
  (:copy  124)
  (:paste  125)
  (:find  126)
  (:mute  127)
  (:volumeup  128)
  (:volumedown  129)
  (:kp-comma  133)
  (:kp-equalsas400  134)

  (:international1  135)
  
  (:international2  136)
  (:international3  137)
  (:international4  138)
  (:international5  139)
  (:international6  140)
  (:international7  141)
  (:international8  142)
  (:international9  143)
  (:lang1  144)
  (:lang2  145)
  (:lang3  146)
  (:lang4  147)
  (:lang5  148)
  (:lang6  149)
  (:lang7  150)
  (:lang8  151)
  (:lang9  152)

  (:alterase  153)
  (:sysreq  154)
  (:cancel  155)
  (:clear  156)
  (:prior  157)
  (:return2  158)
  (:separator  159)
  (:out  160)
  (:oper  161)
  (:clearagain  162)
  (:crsel  163)
  (:exsel  164)

  (:kp-00  176)
  (:kp-000  177)
  (:thousandsseparator  178)
  (:decimalseparator  179)
  (:currencyunit  180)
  (:currencysubunit  181)
  (:kp-leftparen  182)
  (:kp-rightparen  183)
  (:kp-leftbrace  184)
  (:kp-rightbrace  185)
  (:kp-tab  186)
  (:kp-backspace  187)
  (:kp-a  188)
  (:kp-b  189)
  (:kp-c  190)
  (:kp-d  191)
  (:kp-e  192)
  (:kp-f  193)
  (:kp-xor  194)
  (:kp-power  195)
  (:kp-percent  196)
  (:kp-less  197)
  (:kp-greater  198)
  (:kp-ampersand  199)
  (:kp-dblampersand  200)
  (:kp-verticalbar  201)
  (:kp-dblverticalbar  202)
  (:kp-colon  203)
  (:kp-hash  204)
  (:kp-space  205)
  (:kp-at  206)
  (:kp-exclam  207)
  (:kp-memstore  208)
  (:kp-memrecall  209)
  (:kp-memclear  210)
  (:kp-memadd  211)
  (:kp-memsubtract  212)
  (:kp-memmultiply  213)
  (:kp-memdivide  214)
  (:kp-plusminus  215)
  (:kp-clear  216)
  (:kp-clearentry  217)
  (:kp-binary  218)
  (:kp-octal  219)
  (:kp-decimal  220)
  (:kp-hexadecimal  221)

  (:lctrl  224)
  (:lshift  225)
  (:lalt  226)
  (:lgui  227)
  (:rctrl  228)
  (:rshift  229)
  (:ralt  230) 
  (:rgui  231) 

  (:mode  257) 
  
  (:audionext  258)
  (:audioprev  259)
  (:audiostop  260)
  (:audioplay  261)
  (:audiomute  262)
  (:mediaselect  263)
  (:www  264)
  (:mail  265)
  (:calculator  266)
  (:computer  267)
  (:ac-search  268)
  (:ac-home  269)
  (:ac-back  270)
  (:ac-forward  271)
  (:ac-stop  272)
  (:ac-refresh  273)
  (:ac-bookmarks  274)

  (:brightnessdown  275)
  (:brightnessup  276)
  (:displayswitch  277)

  (:kbdillumtoggle  278)
  (:kbdillumdown  279)
  (:kbdillumup  280)
  (:eject  281)
  (:sleep  282)

  (:app1  283)
  (:app2  284)

  (:audiorewind  285)
  (:audiofastforward  286)

  (:softleft  287) 

  (:softright  288)

  (:call  289)
  (:endcall  290)

  (:num-scancodes 512))

(defclass display-event ()
  ())

(defclass window-event ()
  ())

(defclass keyboard-event ()
  ())

(defclass text-editing-event ()
  ())

(defclass text-editing-ext-event ()
  ())

(defclass text-input-event ()
  ())

(defclass mouse-motion-event ()
  ())

(defclass mouse-button-event ()
  ())

(defclass mouse-wheel-event ()
  ())

(defclass joy-axis-event ()
  ())

(defclass joy-ball-event ()
  ())

(defclass joy-hat-event ()
  ())

(defclass joy-button-event ()
  ())

(defclass joy-device-event ()
  ())

(defclass joy-battery-event ()
  ())

(defclass controller-axis-event ()
  ())

(defclass controller-button-event ()
  ())

(defclass controller-device-event ()
  ())

(defclass controller-touchpad-event ()
  ())

(defclass controller-sensor-event ()
  ())

(defclass audio-device-event ()
  ())

(defclass touch-finger-event ()
  ())

(defclass multi-gesture-event ()
  ())

(defclass dollar-gesture-event ()
  ())

(defclass drop-event ()
  ())

(defclass sensor-event ()
  ())

(defclass quit-event ()
  ())

(defclass os-event ()
  ())

(defclass user-event ()
  ())

(defclass sys-wm-event ()
  ())

(c:load-foreign-library
 "/nix/store/n5gjdmcrkslmwvkbbfk0qszm1jp0clvm:SDL2-2.24.2/lib/li:SDL2.so")

(c:defcfun ("SDL_Init" init) :int
  (flags :uint))

(c:defcfun ("SDL_CreateWindow" create-window) :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags :uint))

(c:defcfun ("SDL_DestroyWindow" destroy-window) :void
  (window :pointer))

(c:defcfun ("SDL_GL_CreateContext" gl-create-context) :pointer
  (window :pointer))

(c:defcfun ("SDL_GL_SwapWindow" gl-swap-window) :void
  (window :pointer))

(c:defcfun ("SDL_PollEvent" poll-event) :int
  (event :pointer))

(defun make-event ()
  (c:foreign-alloc :uint8 :count 64))

(defun event-type (event)
  (let ((type (c:mem-ref event :uint32)))
    (c:foreign-enum-keyword 'event type)))

(defparameter +scancode-mask+ (ash 1 30))

(defun scancode->keycode (x)
  (logior x +scancode-mask+))

(c:defcenum keycode
  (:unknown 0)
  (:return  #.(char-code #\return))
  (:escape  #.(char-code #\escape))
  (:backspace  #.(char-code #\backspace))
  (:tab  #.(char-code #\tab))
  (:space  #.(char-code #\space))
  (:exclaim  #.(char-code #\!))
  (:quotedbl  #.(char-code #\"))
  (:hash #.(char-code  #\#))
  (:percent #.(char-code  #\%))
  (:dollar #.(char-code  #\$))
  (:ampersand #.(char-code  #\&))
  (:quote #.(char-code  #\'))
  (:leftparen #.(char-code  #\())
  (:rightparen #.(char-code  #\)))
  (:asterisk #.(char-code  #\*))
  (:plus #.(char-code  #\+))
  (:comma #.(char-code  #\,))
  (:minus #.(char-code  #\-))
  (:period #.(char-code  #\.))
  (:slash #.(char-code  #\/))
  (:0 #.(char-code  #\0))
  (:1 #.(char-code  #\1))
  (:2 #.(char-code  #\2))
  (:3 #.(char-code  #\3))
  (:4 #.(char-code  #\4))
  (:5 #.(char-code  #\5))
  (:6 #.(char-code  #\6))
  (:7 #.(char-code  #\7))
  (:8 #.(char-code  #\8))
  (:9 #.(char-code  #\9))
  (:colon #.(char-code  #\:))
  (:semicolon #.(char-code  #\;))
  (:less #.(char-code  #\<))
  (:equals #.(char-code  #\=))
  (:greater #.(char-code  #\>))
  (:question #.(char-code  #\?))
  (:at #.(char-code  #\@))
  (:leftbracket #.(char-code  #\[))
  (:backslash #.(char-code  #\\))
  (:rightbracket #.(char-code  #\]))
  (:caret #.(char-code  #\^))
  (:underscore #.(char-code  #\-))
  (:backquote #.(char-code  #\`))
  (:a #.(char-code  #\a))
  (:b #.(char-code  #\b))
  (:c #.(char-code  #\c))
  (:d #.(char-code  #\d))
  (:e #.(char-code  #\e))
  (:f #.(char-code  #\f))
  (:g #.(char-code  #\g))
  (:h #.(char-code  #\h))
  (:i #.(char-code  #\i))
  (:j #.(char-code  #\j))
  (:k #.(char-code  #\k))
  (:l #.(char-code  #\l))
  (:m #.(char-code  #\m))
  (:n #.(char-code  #\n))
  (:o #.(char-code  #\o))
  (:p #.(char-code  #\p))
  (:q #.(char-code  #\q))
  (:r #.(char-code  #\r))
  (:s #.(char-code  #\s))
  (:t #.(char-code  #\t))
  (:u #.(char-code  #\u))
  (:v #.(char-code  #\v))
  (:w #.(char-code  #\w))
  (:x #.(char-code  #\x))
  (:y #.(char-code  #\y))
  (:z #.(char-code  #\z))

  (:capslock  #.(scancode->keycode (c:foreign-enum-value 'scancode :capslock)))

  (:f1  #.(scancode->keycode (c:foreign-enum-value 'scancode :f1)))
  (:f2  #.(scancode->keycode (c:foreign-enum-value 'scancode :f2)))
  (:f3  #.(scancode->keycode (c:foreign-enum-value 'scancode :f3)))
  (:f4  #.(scancode->keycode (c:foreign-enum-value 'scancode :f4)))
  (:f5  #.(scancode->keycode (c:foreign-enum-value 'scancode :f5)))
  (:f6  #.(scancode->keycode (c:foreign-enum-value 'scancode :f6)))
  (:f7  #.(scancode->keycode (c:foreign-enum-value 'scancode :f7)))
  (:f8  #.(scancode->keycode (c:foreign-enum-value 'scancode :f8)))
  (:f9  #.(scancode->keycode (c:foreign-enum-value 'scancode :f9)))
  (:f10  #.(scancode->keycode (c:foreign-enum-value 'scancode :f10)))
  (:f11  #.(scancode->keycode (c:foreign-enum-value 'scancode :f11)))
  (:f12  #.(scancode->keycode (c:foreign-enum-value 'scancode :f12)))

  (:printscreen  #.(scancode->keycode (c:foreign-enum-value 'scancode :printscreen)))
  (:scrolllock  #.(scancode->keycode (c:foreign-enum-value 'scancode :scrolllock)))
  (:pause  #.(scancode->keycode (c:foreign-enum-value 'scancode :pause)))
  (:insert  #.(scancode->keycode (c:foreign-enum-value 'scancode :insert)))
  (:home  #.(scancode->keycode (c:foreign-enum-value 'scancode :home)))
  (:pageup  #.(scancode->keycode (c:foreign-enum-value 'scancode :pageup)))
  (:delete  #.(char-code #\Rubout))
  (:end #.(scancode->keycode (c:foreign-enum-value 'scancode :end)))
  (:pagedown  #.(scancode->keycode (c:foreign-enum-value 'scancode :pagedown)))
  (:right  #.(scancode->keycode (c:foreign-enum-value 'scancode :right)))
  (:left  #.(scancode->keycode (c:foreign-enum-value 'scancode :left)))
  (:down  #.(scancode->keycode (c:foreign-enum-value 'scancode :down)))
  (:up  #.(scancode->keycode (c:foreign-enum-value 'scancode :up)))

  (:numlockclear  #.(scancode->keycode (c:foreign-enum-value 'scancode :numlockclear)))
  (:kp-divide  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-divide)))
  (:kp-multiply  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-multiply)))
  (:kp-minus  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-minus)))
  (:kp-plus  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-plus)))
  (:kp-enter  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-enter)))
  (:kp-1  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-1)))
  (:kp-2  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-2)))
  (:kp-3  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-3)))
  (:kp-4  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-4)))
  (:kp-5  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-5)))
  (:kp-6  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-6)))
  (:kp-7  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-7)))
  (:kp-8  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-8)))
  (:kp-9  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-9)))
  (:kp-0  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-0)))
  (:kp-period  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-period)))

  (:application  #.(scancode->keycode (c:foreign-enum-value 'scancode :application)))
  (:power  #.(scancode->keycode (c:foreign-enum-value 'scancode :power)))
  (:kp-equals  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-equals)))
  (:f13  #.(scancode->keycode (c:foreign-enum-value 'scancode :f13)))
  (:f14  #.(scancode->keycode (c:foreign-enum-value 'scancode :f14)))
  (:f15  #.(scancode->keycode (c:foreign-enum-value 'scancode :f15)))
  (:f16  #.(scancode->keycode (c:foreign-enum-value 'scancode :f16)))
  (:f17  #.(scancode->keycode (c:foreign-enum-value 'scancode :f17)))
  (:f18  #.(scancode->keycode (c:foreign-enum-value 'scancode :f18)))
  (:f19  #.(scancode->keycode (c:foreign-enum-value 'scancode :f19)))
  (:f20  #.(scancode->keycode (c:foreign-enum-value 'scancode :f20)))
  (:f21  #.(scancode->keycode (c:foreign-enum-value 'scancode :f21)))
  (:f22  #.(scancode->keycode (c:foreign-enum-value 'scancode :f22)))
  (:f23  #.(scancode->keycode (c:foreign-enum-value 'scancode :f23)))
  (:f24  #.(scancode->keycode (c:foreign-enum-value 'scancode :f24)))
  (:execute  #.(scancode->keycode (c:foreign-enum-value 'scancode :execute)))
  (:help  #.(scancode->keycode (c:foreign-enum-value 'scancode :help)))
  (:menu  #.(scancode->keycode (c:foreign-enum-value 'scancode :menu)))
  (:select  #.(scancode->keycode (c:foreign-enum-value 'scancode :select)))
  (:stop  #.(scancode->keycode (c:foreign-enum-value 'scancode :stop)))
  (:again  #.(scancode->keycode (c:foreign-enum-value 'scancode :again)))
  (:undo  #.(scancode->keycode (c:foreign-enum-value 'scancode :undo)))
  (:cut  #.(scancode->keycode (c:foreign-enum-value 'scancode :cut)))
  (:copy  #.(scancode->keycode (c:foreign-enum-value 'scancode :copy)))
  (:paste  #.(scancode->keycode (c:foreign-enum-value 'scancode :paste)))
  (:find  #.(scancode->keycode (c:foreign-enum-value 'scancode :find)))
  (:mute  #.(scancode->keycode (c:foreign-enum-value 'scancode :mute)))
  (:volumeup  #.(scancode->keycode (c:foreign-enum-value 'scancode :volumeup)))
  (:volumedown  #.(scancode->keycode (c:foreign-enum-value 'scancode :volumedown)))
  (:kp-comma  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-comma)))
  (:kp-equalsas400 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-equalsas400)))

  (:alterase  #.(scancode->keycode (c:foreign-enum-value 'scancode :alterase)))
  (:sysreq  #.(scancode->keycode (c:foreign-enum-value 'scancode :sysreq)))
  (:cancel  #.(scancode->keycode (c:foreign-enum-value 'scancode :cancel)))
  (:clear  #.(scancode->keycode (c:foreign-enum-value 'scancode :clear)))
  (:prior  #.(scancode->keycode (c:foreign-enum-value 'scancode :prior)))
  (:return2  #.(scancode->keycode (c:foreign-enum-value 'scancode :return2)))
  (:separator  #.(scancode->keycode (c:foreign-enum-value 'scancode :separator)))
  (:out  #.(scancode->keycode (c:foreign-enum-value 'scancode :out)))
  (:oper  #.(scancode->keycode (c:foreign-enum-value 'scancode :oper)))
  (:clearagain  #.(scancode->keycode (c:foreign-enum-value 'scancode :clearagain)))
  (:crsel  #.(scancode->keycode (c:foreign-enum-value 'scancode :crsel)))
  (:exsel  #.(scancode->keycode (c:foreign-enum-value 'scancode :exsel)))

  (:kp-00  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-00)))
  (:kp-000  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-000)))
  (:thousandsseparator 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :thousandsseparator)))
  (:decimalseparator 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :decimalseparator)))
  (:currencyunit  #.(scancode->keycode (c:foreign-enum-value 'scancode :currencyunit)))
  (:currencysubunit 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :currencysubunit)))
  (:kp-leftparen  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-leftparen)))
  (:kp-rightparen  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-rightparen)))
  (:kp-leftbrace  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-leftbrace)))
  (:kp-rightbrace  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-rightbrace)))
  (:kp-tab  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-tab)))
  (:kp-backspace  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-backspace)))
  (:kp-a  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-a)))
  (:kp-b  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-b)))
  (:kp-c  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-c)))
  (:kp-d  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-d)))
  (:kp-e  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-e)))
  (:kp-f  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-f)))
  (:kp-xor  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-xor)))
  (:kp-power  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-power)))
  (:kp-percent  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-percent)))
  (:kp-less  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-less)))
  (:kp-greater  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-greater)))
  (:kp-ampersand  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-ampersand)))
  (:kp-dblampersand 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-dblampersand)))
  (:kp-verticalbar 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-verticalbar)))
  (:kp-dblverticalbar 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-dblverticalbar)))
  (:kp-colon  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-colon)))
  (:kp-hash  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-hash)))
  (:kp-space  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-space)))
  (:kp-at  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-at)))
  (:kp-exclam  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-exclam)))
  (:kp-memstore  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memstore)))
  (:kp-memrecall  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memrecall)))
  (:kp-memclear  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memclear)))
  (:kp-memadd  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memadd)))
  (:kp-memsubtract 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memsubtract)))
  (:kp-memmultiply 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memmultiply)))
  (:kp-memdivide  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-memdivide)))
  (:kp-plusminus  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-plusminus)))
  (:kp-clear  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-clear)))
  (:kp-clearentry  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-clearentry)))
  (:kp-binary  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-binary)))
  (:kp-octal  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-octal)))
  (:kp-decimal  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-decimal)))
  (:kp-hexadecimal 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-hexadecimal)))

  (:lctrl  #.(scancode->keycode (c:foreign-enum-value 'scancode :lctrl)))
  (:lshift  #.(scancode->keycode (c:foreign-enum-value 'scancode :lshift)))
  (:lalt  #.(scancode->keycode (c:foreign-enum-value 'scancode :lalt)))
  (:lgui  #.(scancode->keycode (c:foreign-enum-value 'scancode :lgui)))
  (:rctrl  #.(scancode->keycode (c:foreign-enum-value 'scancode :rctrl)))
  (:rshift  #.(scancode->keycode (c:foreign-enum-value 'scancode :rshift)))
  (:ralt  #.(scancode->keycode (c:foreign-enum-value 'scancode :ralt)))
  (:rgui  #.(scancode->keycode (c:foreign-enum-value 'scancode :rgui)))

  (:mode  #.(scancode->keycode (c:foreign-enum-value 'scancode :mode)))

  (:audionext  #.(scancode->keycode (c:foreign-enum-value 'scancode :audionext)))
  (:audioprev  #.(scancode->keycode (c:foreign-enum-value 'scancode :audioprev)))
  (:audiostop  #.(scancode->keycode (c:foreign-enum-value 'scancode :audiostop)))
  (:audioplay  #.(scancode->keycode (c:foreign-enum-value 'scancode :audioplay)))
  (:audiomute  #.(scancode->keycode (c:foreign-enum-value 'scancode :audiomute)))
  (:mediaselect  #.(scancode->keycode (c:foreign-enum-value 'scancode :mediaselect)))
  (:www  #.(scancode->keycode (c:foreign-enum-value 'scancode :www)))
  (:mail  #.(scancode->keycode (c:foreign-enum-value 'scancode :mail)))
  (:calculator  #.(scancode->keycode (c:foreign-enum-value 'scancode :calculator)))
  (:computer  #.(scancode->keycode (c:foreign-enum-value 'scancode :computer)))
  (:ac-search  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-search)))
  (:ac-home  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-home)))
  (:ac-back  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-back)))
  (:ac-forward  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-forward)))
  (:ac-stop  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-stop)))
  (:ac-refresh  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-refresh)))
  (:ac-bookmarks  #.(scancode->keycode (c:foreign-enum-value 'scancode :ac-bookmarks)))

  (:brightnessdown 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :brightnessdown)))
  (:brightnessup  #.(scancode->keycode (c:foreign-enum-value 'scancode :brightnessup)))
  (:displayswitch  #.(scancode->keycode (c:foreign-enum-value 'scancode :displayswitch)))
  (:kbdillumtoggle 
   #.(scancode->keycode (c:foreign-enum-value 'scancode :kbdillumtoggle)))
  (:kbdillumdown  #.(scancode->keycode (c:foreign-enum-value 'scancode :kbdillumdown)))
  (:kbdillumup  #.(scancode->keycode (c:foreign-enum-value 'scancode :kbdillumup)))
  (:eject  #.(scancode->keycode (c:foreign-enum-value 'scancode :eject)))
  (:sleep  #.(scancode->keycode (c:foreign-enum-value 'scancode :sleep)))
  (:app1  #.(scancode->keycode (c:foreign-enum-value 'scancode :app1)))
  (:app2  #.(scancode->keycode (c:foreign-enum-value 'scancode :app2)))

  (:audiorewind  #.(scancode->keycode (c:foreign-enum-value 'scancode :audiorewind)))
  (:audiofastforward  #.(scancode->keycode (c:foreign-enum-value 'scancode :audiofastforward)))

  (:softleft  #.(scancode->keycode (c:foreign-enum-value 'scancode :softleft)))
  (:softright  #.(scancode->keycode (c:foreign-enum-value 'scancode :softright)))
  (:call  #.(scancode->keycode (c:foreign-enum-value 'scancode :call)))
  (:endcall  #.(scancode->keycode (c:foreign-enum-value 'scancode :endcall)))
  )

(c:defcstruct keysym
  (scancode scancode)
  (keycode keycode)
  (mod :uint16))

(c:defcstruct keyboard-event
  (type event)
  (timestamp :uint32)
  (window-id :uint32)
  (state :uint8)
  (repeat :uint8)
  (padding2 :uint8)
  (padding3 :uint8)
  (keysym (:struct keysym)))

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
(loop while (not (eql (event-type *event*) :keydown))
      do (poll-event *event*))

(loop while (plusp (poll-event *event*)))

(make-keyboard-event *event*)

(destroy-window *window*)


