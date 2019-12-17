;Lisp Encryption Tutorial by Chet Allmon is licensed under a Creative Commons Attribution 4.0 International License.

(require racket)

;Simple way of doing the cipher
(define alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
;Purpose: Convert a string to an integer
;Signature: String -> Number
;Examples:
;65-90
;0-65
(check-expect (encode "A") 0)
(check-expect (encode "B") 1)
(check-expect (encode "Z") 25)
;Code
(define (encode letter)
  (- (string->int letter) 65)
  )

; Purpose : Caesar Encrypt Message, 
; Signature : String String -> String
; Example :
(check-expect (caesar-encrypt "B" "A") "B")
(check-expect (caesar-encrypt "B" "C") "D")
(check-expect (caesar-encrypt "B" "Z") "A")
(check-expect (caesar-encrypt "C" "A") "C")
(check-expect (caesar-encrypt "C" "B") "D")
;Stub
;(define (caesar-encrypt key letter) -1)
;Template; 
(define (caesar-encrypt key letter)
  (if (< (+ (encode key) (encode letter)) 25) 
      (string-ith alphabet (+ (encode key) (encode letter)))
      (string-ith alphabet (- (+ (encode key) (encode letter)) 26))
      )
  )

; Purpose: Encrypt an entire message (String with multiple letters)
; Signature: String List -> List
; Examples:
(check-expect (caesar-encrypt-message "B" (list)) (list))
(check-expect (caesar-encrypt-message "B" (list "A" "C" "Z")) (list "B" "D" "A"))
; Stub:
; (define (caesar-encrypt-message key lst) -1)
; Code :
(define (caesar-encrypt-message key lst)
  (if (empty? lst)
      (list)
      (append (list (caesar-encrypt key (first lst)))
              (caesar-encrypt-message key (rest lst))
              )
      )
  )

(implode (caesar-encrypt-message "B" (explode "HELLO")))

(caesar-encrypt-message "N" (list "A" "B" "C"))
(caesar-encrypt-message "N" (caesar-encrypt-message "N" (list "A" "B" "C")))
(caesar-encrypt-message "B" (list "A" "B" "C"))

;Plan
; 1: Introduce Caeser Cipher
; Use numbers for the key
; Using a general encryption function
; Functions: encode-letter, decode-letter, encrypt-letter, decrypt-letter.

;Explain the caeser cipher

;The Caesar Cipher(CC) is a very old cipher. Although it is rather impractical for actual
;security, the Caesar Cipher(CC) is a very simple introduction to encryption and decryption

;The basic use of the CC is to shift the letters of a message to the right in the alphabet.
;In practice,
;Message: "This is super secret"
;When encrypted with a shift of one(1) becomes
;Encrypted Message: "uijt jt tvqfs tfdsfu"

;With a shift of one(1) each letter is shifted to the right once, A->B B->C C->D, ... Y->Z,Z->A,etc.
;This method of encrypting a message makes it very easy to decrypt,
;simply shift back whatever number you used as the shift to encrypt the message

;For a more in-depth explaination of the CC, as well as seeing an encrypter/decrypter program in action, you can visit http://practicalcryptography.com/ciphers/caesar-cipher/

;While you can use whatever shift you desire, even past 26(alphabet size, though this is the same as using a smaller number, I.E 27 shifts is the same as 2 shifts)
;The shift of 13 is the most common, this shift is known as ROT13.
;However, our code will be able to use whatever shift number(key) the user desires to input.

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;Will update this to be better psuedo code/explainations

;The simplest method of encoding a letter is to add the key to the letter itself. (+ "A" 1) However, we cannot add letters to numbers, so we must convert "A" to a number(int)
;We can use (string->int "A") to do this, the result is 65.
;(planning to add information about the acsii table here, to explain why A=65.
;So, with a key of 1, A becomes (+ 65 1) = 66. If we use (int->string 66) we get "B".
;now we can decode this letter, doing the opposite
; B to a number is 66, subtract the key, 65, 65 to a letter is "A".


;Show an encode-one-letter function, show a encrypt-one letter function and then put them together

;Now we can do this in code
;Code

(check-expect (encode-one-letter "A")   65)
(check-expect (encode-one-letter "F")   70)
(check-expect (encode-one-letter "Z")   90)

(define (encode-one-letter letter) 
  (string->int letter))

(check-expect (encrypt-one-letter "A" 1) "B")
(check-expect (encrypt-one-letter "F" 1) "G")
;(check-expect (encrypt-one-letter "Z" 1) "A") ;Throws an error, "A" is expected, returned "["

(define (encrypt-one-letter letter key)
  (int->string (+ (string->int letter) key)))

;The above code works, however, if we try to change "Z", it does not go to "A" like we want. This is because of the ascii table. Z is 90 on the acsii table, And [ is 91.
;So we have to account for the loop around of the alphabet. We can do this using modulo, which returns the remainder of 2 numbers. (modulo 5 3) = 2
;Note that (modulo (0-25) 26) will return the number given (0-25) (modulo 20 26) = 20, etc. However (modulo 26 26) = 0; Which brings us back to "A".

(check-expect (encrypt-one-letter-with-modulo "A" 1) "B")
(check-expect (encrypt-one-letter-with-modulo "F" 1) "G")
(check-expect (encrypt-one-letter-with-modulo "Z" 1) "A") ;No error thrown!

(define (encrypt-one-letter-with-modulo letter key)
  (int->string (+ (string->int "A") (modulo (- (+ (string->int letter) key) (string->int "A")) 26))))

;Note: we can simply replace (string->int "A") with 65, as we know the value, this saves the computer minimal processor power, but it mostly just clears up the code a bit.
;This is a matter of personal preference, if you prefer having the calculation to show where 65 came from feel free, you can also just add a comment to explain the 65.
;Ex:

;Origin of 65 in the following function: (string->int "A") = 65
;(define (encode-one-letter-with-modulo letter key)
;  (int->string (+ 65 (modulo (- (+ (string->int letter) key) 65) 26))))
                       
;---------------------------------------------------------------
;potential answer when asking reader to decode/decrypt, this code is just the reverse of the above code

(check-expect (decode-one-letter "A")   65)
(check-expect (decode-one-letter "F")   70)
(check-expect (decode-one-letter "Z")   90)

;NOTE: this is THE SAME FUNCTION as the encode function. 
(define (decode-one-letter letter) 
  (string->int letter))

(check-expect (decrypt-one-letter "B" 1) "A")
(check-expect (decrypt-one-letter "F" 1) "E")
;(check-expect (decrypt-one-letter "A" 1) "Z") ;Throws an error, "A" is expected, returned "@"

(define (decrypt-one-letter letter key)
  (int->string (- (string->int letter) key)))

(check-expect (decrypt-one-letter-with-modulo "B" 1) "A")
(check-expect (decrypt-one-letter-with-modulo "G" 1) "F")
(check-expect (decrypt-one-letter-with-modulo "A" 1) "Z") ;No error thrown!

(define (decrypt-one-letter-with-modulo letter key)
  (int->string (+ (string->int "A") (modulo (- (- (string->int letter) key) (string->int "A")) 26))))

(encrypt-one-letter-with-modulo "A" 1)
(decrypt-one-letter-with-modulo "B" 1)


;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; 2: How to encrypt a String = list of 1String

;Perhaps show method of doing it without lambda/currying...from old project.



; Cannot use the general encryption function on a list with map because it uses two paramaters
; Introduce lambda, map, then currying
; a. Create function to use with map
; b. Introduce lambda as a concept
; c. Use a lambda with map
; d. Use lambda with general encrypt-letter using currying to create a function that can be used with map.
; e. Put the whole thing together...

;Explaining a,b,c, here.


;Step2-Putting it all together
;Code:

;Functions to be made, encode-character, decode-character, encrypt-message,decrypt-message

(check-expect ((encode-character-function 1) "A") "B") 

(define (encode-character-function key)
  (lambda(letter) (int->string (+ (string->int "A") (modulo (- (+ (string->int letter) key) (string->int "A")) 26)))))

(define (encrypt-message message key)
  (implode (map (encode-character-function key) (explode message))))

(encrypt-message "HELLO" 1)

(define (decode-character-function key)
  (lambda(letter) (int->string (+ (string->int "A") (modulo (+ (- (string->int letter) key) (string->int "A")) 26)))))
 
(define (decrypt-message message key)
  (implode (map (decode-character-function key) (explode message))))

(decrypt-message "IFMMP" 1)

(encrypt-message "HOWAREYOU" 5)
(decrypt-message "MTBFWJDTZ" 5)

(encrypt-message "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 1)
(decrypt-message "BCDEFGHIJKLMNOPQRSTUVWXYZA" 1)


;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;
; 3. Modern encryption - using random numbers
; give the user the ability to enter the seed.

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;maybe create a random number generator, donald knuth "The art of computer programming"
;sequential block
(define key 10)

(random-seed key)


(define (lock) (random 26))

lock ; This gives the value (key) to shift the messages characters by. 

(check-expect (encode-char "B") "E")
(check-expect (decode-char "E") "B")


(define (encode-char char)
  (int->string (+ (string->int "A") (modulo (- (+ (string->int char) (lock)) (string->int "A")) 26))))

(define (decode-char char)
  (int->string (+ (string->int "A") (modulo (- (- (string->int char) (lock)) (string->int "A")) 26))))



(check-expect ((encode-char-function) "A") "D") 

(define (encode-char-function)
  (lambda(letter) (int->string (+ (string->int "A") (modulo (- (+ (string->int letter) (lock)) (string->int "A")) 26)))))

(define (encrypt-message-rand message)
  (implode (map (encode-char-function) (explode message))))


(encrypt-message-rand "HELLO")

(check-expect ((decode-char-function) "D") "A") 

(define (decode-char-function)
  (lambda(letter) (int->string (+ (string->int "A") (modulo (+ (- (string->int letter) (lock)) (string->int "A")) 26)))))
 
(define (decrypt-message-rand message)
  (implode (map (decode-char-function) (explode message))))


(random-seed key)
(decrypt-message-rand "KJLEY")
"-------------------"
(random-seed key)
(encrypt-message-rand "AAAAA")
(random-seed key)
(decrypt-message-rand "DFATK")
               

(set! key 5) ; This redefines key to be 5
(random-seed key)
"----------------------"
(encrypt-message-rand "HELLO")
(random-seed key)
(decrypt-message-rand "RHGJX")


                     

;----An Introduction to Anonymous Functions----

;A standard function
;
;(define (times-five x)
;  (* x 5))
;
;;Calling the function
;(times-five 4)
;;Outputs -> 20
;
;;A standard function used in another function
;
;(map times-five
;     (list 1 2 3))
;
;;Outputs -> (list 5 10 15)
;
;;An Anonymous function used in a lambda
;
;(map (lambda(x)
;       (* x 5))
;     (list 1 2 3))
            
;Outputs -> (list 5 10 15)

;Note:
;That (* x 5) is not defined anywhere.
;This anonymous function (* x 5) is not declared or used anywhere else.
;This allows for quick, one time functions, to be created and used without cluttering up your code.
;It can also speed up your program significantly on large-scale programs (compared to defineing a function each time).
;Note that anonymous functions are created at run-time.



;(define (myfunction a b c)
;  42
;  )
; 
;(define (make-multiply-function constant)
;  (lambda (x) (* constant x))
;  )
;
;(define f (make-multiply-function 17))
;(define g (make-multiply-function 27))
;
;(define a-list (list 1 2 3))
;
;(map f a-list)
;(map g a-list)
;
;(define (make-encryption-function key)
;  (λ(x) (+ x key)
;    )
;  )
;
;(define (make-decryption-function key)
;    (λ(x) (- x key)
;    )
;  )
;
;
;(define (encrypt letter key)
;  (modulo (+ letter key) 25)
;  )
;
;;; Currying: turn a general function 
;(define (make-encrypt-letter-function key)
;  (lambda(letter) (encrypt letter key))
;  )
;
;(map (make-encrypt-letter-function 13) (list 104 101))
;
;
;
;(map string->int (explode "hello"))
;(map (make-encryption-function 13) (map string->int (explode "hello")))
;(map int->string (map (make-encryption-function 13) (map string->int (explode "hello"))))
;(implode (map int->string (map (make-encryption-function 3) (map string->int (explode "hello world")))))
;
;(implode (map int->string (map (make-decryption-function 3) (map string->int (explode "uryy|-\u0084|\u007Fyq")))))


;Plan
; 1: Introduce Caeser Cipher
; Use numbers for the key
; Using a general encryption function
; Functions: encode-letter, decode-letter, encrypt-letter, decrypt-letter.
; 
; 2: How to encrypt a String = list of 1String
; Cannot use the general encryption function on a list with map because it uses two paramaters
; Introduce lambda, map, then currying
; a. Create function to use with map
; b. Introduce lambda as a concept
; c. Use a lambda with map
; d. Use lambda with general encrypt-letter using currying to create a function that can be used with map.
; e. Put the whole thing together...
;
; 3. Modern encryption - using random numbers
; give the user the ability to enter the seed.
