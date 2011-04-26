;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Euler-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define composite 600851475143)

;; divides?: Number Number -> Boolean
;; Returns true if a divides b
(define (divides? a b)
  (zero? (modulo a b)))

(check-expect (divides? 4 2) true)
(check-expect (divides? 5 8) false)

;; get-candidates: Number -> [Listof Numbers]
;; Returns a list from 2 to n
(define (get-candidates n)
  (rest (build-list n add1)))
(check-expect (get-candidates 10) (list 2 3 4 5 6 7 8 9 10))

;; sieve: [Listof Numbers] -> [Listof Primes]
;; Sieves a list of numbers for its primes
(define (sieve lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (cons (first lst)
                       (sieve (filter (λ (i) (not (divides? i (first lst))))
                                      (rest lst))))]))
(check-expect (sieve (get-candidates 10)) (list 2 3 5 7))


;; greatest-prime: [Listof Primes] Number -> Number
;; Returns the largest prime number
(define (greatest-prime lst n)
  (foldl max 1 (filter (λ (i) (divides? n i))
                     lst)))

(check-expect (greatest-prime (sieve (get-candidates 10)) 10) 5)

;; euler: Number -> Number
;; Takes a number and returns its greatest prime factor.
(define (euler n)
  (greatest-prime (sieve (get-candidates (integer-sqrt n))) n))
;; Takes 42 minutes to find the greatest prime factor of the composite.
;; Returns the correct number.

;; euler.v2: Number -> Number
;; Instead of going over the same lists multiple times,
;;   I'm going to do it all in one shot.
(define (euler.v2 n)))

;; Run
(time (euler composite))
