;;;; file: t/bit-smasher.lisp

(in-package :cl-user)

(defpackage bit-smasher-test
  (:use cl bit-smasher prove))

(in-package :bit-smasher-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bit-smasher)' in your Lisp.

(plan 5)

(deftest sanity-check
  (pass "PROVE is loaded and ready to go.")
  (ok (= 1 1)
      "Numeric equality: (= 1 1) => T.")
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4.")
  (is (mod (+ 10 2) 10)
      2
      "Modulus: (mod (+ 10 2) 10) => 2."))

;; Bit-Vector Arithmetic Test

(deftest arithmetic
  (is (bit+ #*0010 #*1000)
      #*00001010
      :test #'equal
      "Sum of two bit-vectors.")
  (is (bit- #*1000 #*0010)
      #*00000110
      :test #'equal
      "Difference of two bit-vectors.")
  (is (bit* #*1000 #*0010)
      #*00010000
      :test #'equal
      "Product of two bit-vectors.")
  (is (bit/ #*1000 #*0010)
      #*00000100
      :test #'equal
      "Quotient of two bit-vectors.")
  (is (bit-floor #*1000 #*0010)
      #*00000100
      :test #'equal
      "Floor-Division of two bit-vectors.")
  (is (bit-ceiling #*1000 #*0010)
      #*00000100
      :test #'equal
      "Ceiling-Division of two bit-vectors.")
  (is (<< #*0010 2)
      #*00001000
      :test #'equal
      "Left-shift Bit-Vector by two bits.")
  (is (<< "FF" 2)
      #*0000001111111100
      :test #'equal
      "Left-shift Hex-String by two bits.")
  (is (<< (octets<- 255) 2)
      #*0000001111111100
      :test #'equal
      "Left-shift Octets by two bits.")
  (is (<< 255 2)
      #*0000001111111100
      :test #'equal
      "Left-shift Integer by two bits.")
  (is-error (<< #(255) 2)
            'type-error
            "LEFT-SHIFT error triggered: type of N not recognized.")
  (is (>> #*1000 2)
      #*00000010
      :test #'equal
      "Right-shift Bit-Vector by two bits.")
  (is (>> "FF" 2)
      #*00111111
      :test #'equal
      "Right-shift Hex-String by two bits.")
  (is (>> (octets<- 255) 2)
      #*00111111
      :test #'equal
      "Right-shift Octets by two bits.")
  (is (>> 255 2)
      #*00111111
      :test #'equal
      "Right-shift Integer by two bits.")
  (is-error (>> #(255) 2)
            'type-error
            "RIGHT-SHIFT error triggered: type of N not recognized."))

;; Conversion test

(deftest conversion
  (is (hex<- 0)
      "00"
      "Zero to Hex-String conversion.")
  (is (hex<- (expt 10 64))
      "184f03e93ff9f4daa797ed6e38ed64bf6a1f010000000000000000"
      "Integer to Hex-String conversion.")
  (is (hex<- (octets<- 255))
      "FF"
      :test #'equalp
      "Octets to Hex-String conversion.")
  (is (hex<- "FF")
      "FF"
      "Hex-String self-check.")
  (is-error (hex<- #(0))
            'type-error
            "HEX<- error triggered: type of N not recognized.")
  (is (bits<- 0)
      #*00000000
      :test #'equal
      "Zero to Bit-Vector conversion.")
  (is (bits<- (expt 10 64))
      #*000110000100111100000011111010010011111111111001111101001101101010100111100101111110110101101110001110001110110101100100101111110110101000011111000000010000000000000000000000000000000000000000000000000000000000000000
      "Integer to Bit-Vector conversion.")
  (is (bits<- "FF")
      #*11111111
      "Hex-String to Bit-Vector conversion.")
  (is (bits<- (octets<- "FF"))
      #*11111111
      "Octets to Bit-Vector conversion.")
  (is (bits<- #*1000)
      #*1000
      "Bit-Vector self-check.")
  (is-error (bits<- #(0))
            'type-error
            "BITS<- error triggered: type of N not recognized.")
  (is (octets<- (expt 10 64))
      #(24 79 3 233 63 249 244 218 167 151 237 110 56 237 100 191 106 31 1 0 0 0 0 0 0 0 0)
      :test #'equalp
      "Integer to Octets conversion.")
  (is (octets<- 0)
      #(0)
      :test #'equalp
      "Zero to Octets conversion.")
  (is (octets<- #*11111111)
      #(255)
      :test #'equalp
      "Bit-Vector to Octets conversion.")
  (is (octets<- "FF")
      #(255)
      :test #'equalp
      "Hex-String to Octets conversion.")
  (is (octets<- (octets<- 255))
      #(255)
      :test #'equalp
      "Octets self-check.")
  (is-error (octets<- #(0))
            'type-error
            "OCTETS<- error triggered: type of N not recognized.")
  (is (int<- (bits<- (expt 10 64)))
      (expt 10 64)
      "Bit-Vector to Integer conversion.")
  (is (int<- 0)
      0
      "Zero integer conversion test.")
  (is (int<- "FF")
      255
      "Hex-String to Integer conversion.")
  (is (int<- (octets<- 255))
      255
      "Octets to Integer conversion.")
  (is (int<- 255)
      255
      "Integer self-check.")
  (is-error (int<- #(0))
            'type-error
            "INT<- error triggered: type of N not recognized.")
  (ok (string-equal (hex<- (bits<- (expt 10 64)))
                    (hex<- (expt 10 64)))
      "Bit-Vector to Hex-String conversion.")
  (ok (equalp (octets<- (bits<- (expt 10 64)))
              (octets<- (expt 10 64)))
      "Bit-Vector to Octets conversion."))

;; Two's Complement Test

(deftest twos-complement
  (ok (twos-complement-p 0)
      "0 is Two's Complement.")
  (ok (not (twos-complement-p 1))
      "1 is not Two's Complement.")
  (ok (twos-complement-p 2)
      "2 is Two's Complement."))

(deftest bit-length
  (is (byte-length 255)
      1
      "Byte-length of integer.")
  (is (byte-length "FF")
      1
      "Byte-length of Hex-String.")
  (is (byte-length #*11111111)
      1
      "Byte-length of Bit-Vector.")
  (is (byte-length (octets<- 255))
      1
      "Byte-length of Octets.")
  (is-error (byte-length #(255))
            'type-error
            "Byte-Length error triggered: type of N not recognized.")
  (is (wide-bit-length 255)
      8
      "Wide Bit-Length of Integer.")
  (is (wide-bit-length "FF")
      8
      "Wide Bit-Length of Hex-String.")
  (is (wide-bit-length #*11111111)
      8
      "Wide Bit-Length of Bit-Vector.")
  (is (wide-bit-length (octets<- 255))
      8
      "Wide Bit-Length of Octets.")
  (is-error (wide-bit-length #(255))
            'type-error
            "Wide Bit-Length error triggered: type of N not recognized.")
  (is (min-bit-length 255)
      8
      "Minimum Bit-Length of Integer.")
  (is (min-bit-length "FF")
      8
      "Minimum Bit-Length of Hex-String.")
  (is (min-bit-length #*11111111)
      8
      "Minimum Bit-Length of Bit-Vector.")
  (is (min-bit-length (octets<- 255))
      8
      "Minimum Bit-Length of Octets.")
  (is-error (min-bit-length #(255))
            'type-error
            "Minimum Bit-Length error triggered: type of N not recognized."))

(run-test-all)

;; EOF
