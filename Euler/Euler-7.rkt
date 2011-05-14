;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Project Euler - Problem #7
;; Solved by Ali Ukani on May 14, 2011
;; "Find the 10001st prime."
(require racket)

;; divides? : Number Number -> Boolean
;; Returns true if a divides b (if remainder of a/b is 0).
(define (divides? a b)
  (zero? (modulo a b)))

;; nth-prime : Number -> Number
;; Finds the nth prime for a given n.
;;   euclid : Number Number [Listof Numbers] -> [Listof Numbers]
;;   Returns a list of prime numbers up to the nth prime.
(define (nth-prime n)
  (local [(define (euclid x n acc)
            (if (= (length acc) n) acc
                (cond
                  [(ormap (λ (i) (divides? x i)) acc) (euclid (add1 x) n acc)]
                  [else (euclid (add1 x) n (append acc (list x)))])))]
    (last (euclid 2 n empty))))
(check-expect (nth-prime 1) 2)
(check-expect (nth-prime 25) 97)

;; Solve
(nth-prime 10001)