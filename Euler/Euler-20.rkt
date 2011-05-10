;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Project Euler - Problem #20
;; Solved by Ali Ukani on May 10, 2011
;; "Find the sum of digits in 100!"

;; factorial : Number -> Number
;; n! = n*(n-1)*...*3*2*1, and 0! = 1.
(define (factorial n)
  (cond
   [(zero? n) 1]
   [else (* n (factorial (sub1 n)))]))
(check-expect (factorial 0) 1)
(check-expect (factorial 1) 1)
(check-expect (factorial 10) 3628800)

;; sum-of-digits : Number -> Number
;; Returns the sum of a given number's digits
(define (sum-digits n)
  (cond
    [(< n 10) n]
    [else (+ (modulo n 10)
             (sum-digits (floor (/ n 10))))]))
(check-expect (sum-digits 3628800) 27)
(check-expect (sum-digits 0) 0)

;; Solve
(sum-digits (factorial 100))