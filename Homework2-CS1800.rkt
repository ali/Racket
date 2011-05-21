;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Homework2-CS1800) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; This a simple linear cypher program I wrote
;; to solve a problem from my freshman year
;; Discrete Structures class. I really didn't want to
;; manually map characters by hand, so I made this.

;; I wrote this before I really got the hang of Racket,
;; so the code is kind of embarassing.
;; Putting it up anyway. Haters gonna hate.
;; - Ali

;; The starting message
(define message "e juczehrey tebia puju xer ercijressej")

;; Procedural wishlist:
;; String -> List (explode message) => (listof message)
;; String -> Character [ex. (string-ref "a" 0) => #\a ]
;; Character -> Number (encode)

;; encode : String -> Number
;; Returns a letter's position in the alphabet
(define (encode a)
  (- (char->integer (string-ref a 0))
     (char->integer #\a)))
(check-expect (encode "a") 0)
(check-expect (encode "z") 25)

;; decode : Number => String
;; Returns a letter by its position in the alphabet
(define (decode a)
  (make-string 1 (integer->char (+ a (char->integer #\a)))))
(check-expect (decode 0) "a")
(check-expect (decode 25) "z")

;; Keys for linear encryption
(define M 17)
(define K 4)
(define TOTIENT 12) ;; Euler's Totient of 26 => 26 * (1 - 1/13) * (1 - 1/2) = 12
(define INV (modulo (expt M (- TOTIENT 1)) 26)) ;; Multiplicative inverse of M

;; encrypt : Number -> Number
;; Using constants M & K, uses a linear cypher to encrypt a number (a).
;; a → (m * a + k) mod 26.
(define (encrypt a)
  (modulo (+ (* M a)
             K)
          26))

;; decrypt : Number -> Number
;; Using constants M & K, uses a linear cypher to decrypt a number (a).
;; (a - k) * multiplicative inverse of M
(define (decrypt a)
  (modulo (* (- a K)
             INV)
          26))

;; decipher : String -> String
;; Deciphers a letter by encoding, decrypting, and then decoding it.
;; If given a space (" "), return a space. Else, decipher. 
(define (decipher a)
  (if (string=? a " ") " "
      (decode (decrypt (encode a)))))

;; Decipher
(implode (map decipher (explode message)))