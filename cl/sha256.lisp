(require 'sb-rotate-byte)
(defpackage sha256 (:use :cl :sb-rotate-byte) (:export :sha256))
(in-package sha256)

;; Note 1: All variables are 32 bit unsigned integers and addition is calculated modulo 232
;; Note 2: For each round, there is one round constant k[i] and one entry in the message schedule array w[i], 0 ≤ i ≤ 63
;; Note 3: The compression function uses 8 working variables, a through h
;; Note 4: Big-endian convention is used when expressing the constants in this pseudocode,
;;     and when parsing message block data from bytes to words, for example,
;;     the first word of the input message "abc" after padding is 0x61626380

;; Initialize hash values:
;; (first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19):
(declaim (type (unsigned-byte 32) +h0+ +h1+ +h2+ +h3+ +h4+ +h5+ +h6+ +h7+))
(defconstant +h0+ #x6a09e667)
(defconstant +h1+ #xbb67ae85)
(defconstant +h2+ #x3c6ef372)
(defconstant +h3+ #xa54ff53a)
(defconstant +h4+ #x510e527f)
(defconstant +h5+ #x9b05688c)
(defconstant +h6+ #x1f83d9ab)
(defconstant +h7+ #x5be0cd19)

;; Initialize array of round constants:
;; (first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311):
(declaim (type (simple-array (unsigned-byte 32) (64)) +k+))
(defconstant +k+
  (make-array 64
    :element-type '(unsigned-byte 32)
    :initial-contents
    #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
      #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
      #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
      #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
      #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
      #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
      #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
      #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

;; Pre-processing (Padding):
;; begin with the original message of length L bits
;; append a single '1' bit
;; append K '0' bits, where K is the minimum number >= 0 such that (L + 1 + K + 64) is a multiple of 512
;; append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
;; such that the bits in the message are: <original message of length L> 1 <K zeros> <L as 64 bit integer> , (the number of bits will be a multiple of 512)
;; (declaim (ftype (function ((simple-array (unsigned-byte 8) (*))) (simple-array (unsigned-byte 8) (*))) pad))
(declaim (ftype (function ((simple-array (unsigned-byte 8) (*))) (simple-array bit (*))) pad))
(defun pad (msg)
  (let* ((L (* 8 (length msg)))
         (K (do ((k 0 (1+ k))) ((zerop (mod (+ L 1 k 64) 512)) k)))
         (padding (make-array (+ 1 K 64) :element-type 'bit :initial-element 0)))
    ;; (declare (type (mod 512) K))
    ;; (declare (dynamic-extent padding))
    (setf (aref padding 0) 1)
    (do ((bit 63 (1- bit)))
        ((zerop bit) padding)
      (setf (aref padding (+ 1 K (abs (- bit 64)))) (ldb (byte 1 bit) L)))))

;; (time (loop for x below 10000
;;   always (zerop (mod (length (pad (make-array x :element-type '(unsigned-byte 8) :initial-contents (loop repeat x collect (random 255))))) 8))))

;; Process the message in successive 512-bit chunks:
;; break message into 512-bit chunks
(declaim (ftype (function ((simple-array (unsigned-byte 32) (16))) vector) update))
(defun update (chunk)
  ;; (declare (optimize (safety 0)))
  ;; for each chunk
  ;; create a 64-entry message schedule array w[0..63] of 32-bit words
  ;; (The initial values in w[0..63] don't matter, so many implementations zero them here)
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0))
        (h0 +h0+)
        (h1 +h1+)
        (h2 +h2+)
        (h3 +h3+)
        (h4 +h4+)
        (h5 +h5+)
        (h6 +h6+)
        (h7 +h7+))
    (declare (dynamic-extent w))
    (declare (type (unsigned-byte 32) h0 h1 h2 h3 h4 h5 h6 h7))
    ;; TODO Maybe just mask it out at the end?
    (flet ((+u32 (&rest is)
             (logand #xffffffff (apply #'+ is))))
    (declare (ftype (function (&rest (unsigned-byte 32)) (unsigned-byte 32)) +u32))
    ;; copy chunk into first 16 words w[0..15] of the message schedule array
    (replace w chunk)
    ;; Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
    (do ((i 16 (1+ i)))
        ((= i 64))
      (let ((s0 (logxor (rotate-byte -7 (byte 32 0) (aref w (- i 15)))
                        (rotate-byte -18 (byte 32 0) (aref w (- i 15)))
                        (ash (aref w (- i 15)) -3)))
            (s1 (logxor (rotate-byte -17 (byte 32 0) (aref w (- i 2)))
                        (rotate-byte -19 (byte 32 0) (aref w (- i 2)))
                        (ash (aref w (- i 2)) -10))))
        (setf (aref w i) (+u32 (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
    ;; Initialize working variables to current hash value:
    (let ((a h0)
          (b h1)
          (c h2)
          (d h3)
          (e h4)
          (f h5)
          (g h6)
          (h h7))
      (declare (type (unsigned-byte 32) a b c d e f g h))
      ;; Compression function main loop:
      (dotimes (i 64)
        (let* ((S1 (logxor (rotate-byte -6 (byte 32 0) e)
                           (rotate-byte -11 (byte 32 0) e)
                           (rotate-byte -25 (byte 32 0) e)))
               (ch (logxor (logand e f) (logand (lognot e) g)))
               (temp1 (+u32 h S1 ch (aref +k+ i) (aref w i)))
               (S0 (logxor (rotate-byte -2 (byte 32 0) a)
                           (rotate-byte -13 (byte 32 0) a)
                           (rotate-byte -22 (byte 32 0) a)))
               (maj (logxor (logand a b) (logand a c) (logand b c)))
               (temp2 (+u32 S0 maj)))
          (setf h g
                g f
                f e
                e (+u32 d temp1)
                d c
                c b
                b a
                a (+u32 temp1 temp2))))
      ;; Add the compressed chunk to the current hash value:
      (setf h0 (+u32 h0 a))
      (setf h1 (+u32 h1 b))
      (setf h2 (+u32 h2 c))
      (setf h3 (+u32 h3 d))
      (setf h4 (+u32 h4 e))
      (setf h5 (+u32 h5 f))
      (setf h6 (+u32 h6 g))
      (setf h7 (+u32 h7 h))))
    ;; Produce the final hash value (big-endian):
    (vector h0 h1 h2 h3 h4 h5 h6 h7)))

(time (update (make-array 16
          :element-type '(unsigned-byte 32)
          :initial-contents
          (loop repeat 16 collect 0))))

(disassemble 'update)

;; 00000000 11111111 11111111 11111111 11111111 = #xFFFFFFFF
;; 00000000 10000000 00000000 00000000 00000000 = #x80000000
;; +
;; 00000001 01111111 11111111 11111111 11111111 = #x017FFFFFFF


(disassemble
 (lambda (x)
   (declare (type (unsigned-byte 32) x))
   (mod (+ x #x80000000) (1- (expt 2 31)))))

(logand #xffffffff00000000 (+ #xffffffff #x80000000))

(disassemble (lambda (x) (declare (type (unsigned-byte 32) x)) (logbitp 32 x)))

(disassemble (lambda (x) (declare (type (unsigned-byte 32) x))
               (dpb 0 (byte 1 31) (ash (+ x #xffffffff #x0f00000f) -1))))

(disassemble (lambda (x) 
               (declare (type (unsigned-byte 32) x))
               (logand #xffffffff (+ (+ x #x0fffffff) #x0f00000f))))

;; #xffffffff = 4294967295
;; #x0f00000f = 251658255
;; +          = 4546625550
;; (mod 4546625550 (expt 2 32))
(dpb 0 (byte 1 32) (+ #xffffffff #x0f00000f))

