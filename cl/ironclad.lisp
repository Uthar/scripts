
(crypto:list-all-ciphers)

(defparameter *xtea*
  (crypto:make-cipher :xtea
                      :key (make-array 16 :element-type '(integer 0 255)
                                          :initial-element (random 100))
                      :mode :ecb))


(crypto:list-all-kdfs)

(defparameter msg #(10 20 30 40 50 60 70 80))

(crypto:encrypt-in-place *xtea* msg)

(crypto:decrypt-in-place *xtea* msg)
