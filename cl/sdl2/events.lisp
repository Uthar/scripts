(defpackage sdl2/events
  (:use :cl)
  (:import-from :sdl2/keyboard :keysym)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl2/events)

(c:defcenum event-type
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

(c:defcstruct common-event
  (type event-type)
  (timestamp :uint32))

(c:defcstruct display-event
  (type event-type)
  (timestamp :uint32)
  (display :uint32)
  (event :uint8)
  (padding1 :uint8)
  (padding2 :uint8)
  (padding3 :uint8)
  (data1 :int32))

(c:defcstruct window-event
  (type event-type)
  (timestamp :uint32)
  (window-id :uint32)
  (event :uint8)
  (padding1 :uint8)
  (padding2 :uint8)
  (padding3 :uint8)
  (data1 :int32)
  (data2 :int32))

(c:defcstruct keyboard-event
  (type event-type)
  (timestamp :uint32)
  (window-id :uint32)
  (state :uint8)
  (repeat :uint8)
  (padding2 :uint8)
  (padding3 :uint8)
  (keysym (:struct keysym)))

(c:defcstruct mouse-motion-event
  (type event-type)
  (timestamp :uint32)
  (window-id :uint32)
  (which :uint32)
  (state :uint32)
  (x :int32)
  (y :int32)
  (xrel :int32)
  (yrel :int32))

(c:defcstruct mouse-button-event
  (type event-type)
  (timestamp :uint32)
  (window-id :uint32)
  (which :uint32)
  (button :uint8)
  (state :uint8)
  (clicks :uint8)
  (padding1 :uint8)
  (x :int32)
  (y :int32))

(c:defcstruct mouse-wheel-event
  (type event-type)
  (timestamp :uint32)
  (window-id :uint32)
  (which :uint32)
  (x :int32)
  (y :int32)
  (direction :uint32)
  (precise-x :float)
  (precise-y :float))

(defun make-event ()
  (c:foreign-alloc :uint8 :count 64))

(c:defcfun ("SDL_PollEvent" poll-event) :int
  (event :pointer))
