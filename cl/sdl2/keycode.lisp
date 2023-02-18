(defpackage sdl2/keycode
  (:use :cl)
  (:import-from :sdl2/scancode :scancode)
  (:local-nicknames
   (:c :cffi)))

(in-package sdl2/keycode)

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
  (:kp-equalsas400  #.(scancode->keycode (c:foreign-enum-value 'scancode :kp-equalsas400)))

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
