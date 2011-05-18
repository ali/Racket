;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #16
;; Solved by Ali Ukani on May 18, 2011
;; "2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26."
;; "What is the sum of the digits of the number 2^1000?"

;; sum-digits : Number -> Number
;; Sums the digits of a given number
(define (sum-digits n)
  (cond
    [(< n 10) n]
    [else (+ (modulo n 10)
             (sum-digits (floor (/ n 10))))]))

(check-expect (sum-digits 32768) 26)

;; Solve
(sum-digits (expt 2 1000))