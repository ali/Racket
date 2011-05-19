;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #25
;; Solved by Ali Ukani on May 19, 2011
;; "What is the first term in the Fibonacci sequence to contain 1000 digits?"

;; 3 digits -> 100; n > (10^(3-1) -1)

(define lim (- (expt 10 999) 1))
;; Returns true if n has over 1000 digits
(define (stop? n) (> n lim))

;; a is term c-2
;; b is term c-1
;; c is the current term
;; t is what term c (c = a + b) is in the Fibbonacci sequence
;; Ex: a = 0, b = 1, c = 1, t = 2
(define (solve a b t)
  ((λ (c)(if (stop? c) t
             (solve b c (+ 1 t))))
   (+ a b)))

(solve 0 1 2)