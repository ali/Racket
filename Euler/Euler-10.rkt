;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #10
;; Solved by Ali Ukani on May 17, 2011
;; "Calculate the sum of all the primes below two million."

(require racket)

;; divides?: Number Number -> Boolean
;; Returns true if a divides b
(define (divides? a b)
  (= (modulo a b) 0))

(check-expect (divides? 4 2) true)
(check-expect (divides? 5 8) false)

;; sieve: Number -> [Listof Primes]
;; Returns a list of all prime numbers from 2 to n
;;   1. Make list of numbers from 1 to n (and then drop the '1')
;;   2. Remove all numbers divisible by the first number
;;   3. Repeat step 2 for each of the next numbers until we get to a number > (sqrt n)
(define (sieve n)
  (local
    [(define lst (rest (build-list n add1)))
     (define (do lst)
       (cond
         [(> (first lst) (floor (sqrt n))) lst]
         [(cons (first lst)
                (do (filter (λ (i) (not (divides? i (first lst)))) lst)))]))]
    (do lst)))
(check-expect (sieve 10) '(2 3 5 7))

;; Solve
(time (foldl + 0 (sieve 2000000)))