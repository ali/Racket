;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require racket)
;; Project Euler - Problem #12
;; Solved by Ali Ukani on May 18, 2011
;; "What is the value of the first triangle number to have over five hundred divisors?"

;; divides? : Number Number -> Boolean
;; Returns true if a divides b.
(define (divides? a b)
  (= (modulo a b) 0))
(check-expect (divides? 4 2) true)
(check-expect (divides? 5 3) false)

;; get-nth-triangle : Number -> Number
;; Computes the nth triangle number, given n.
;;   A triangle number is the additive analogy of a factorial
;;   1 + 2 + ... + (n-1) + n = nth triangle
;;   Hey! It's the sum of an arithmetic series!
(define (get-nth-triangle n)
  (* (/ n 2) (+ 1 n)))
(check-expect (get-nth-triangle 7) 28)
(check-expect (get-nth-triangle 10) 55)

;; divisors : Number -> Number
;; Returns the number of divisors of a given number.
(define (divisors n)
  (* 2 (length (filter (λ (x) (divides? n x))
                       (build-list (integer-sqrt n) add1)))))
(check-expect (divisors 21) 4)
(check-expect (divisors 28) 6)

;; Solve

;; If a triangle number has <500 divisors, look at the next number.
;; Return the first triangle number with >500 divisors.
(define (euler i)
  ((λ (t) (if (< (divisors t) 500)
             (euler (+ 1 i))
             t))
   (get-nth-triangle i)))

;; "We can see that 28 is the first triangle number to have over five divisors."
;; Because we *know* 28 is the 7th triangle number, we'll start there.
;;                                   (but I could safely start at 400)
(euler 7)

;; Takes 11.3 seconds to compute.