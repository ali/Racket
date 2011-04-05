;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Euler-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Project Euler - Problem 1      (Ali Ukani - 02/26/2011)
;; Find the sum of all the multiples of 3 or 5 below 1000.

(define LIMIT (- 1000 1))

;; get-multiples : Number => Number
;; Returns the sum of all multiples of a given number that are less than LIMIT.
;;  (Sum of a finite arithmetic progression)
(define (get-multiples x)
  (* (/ x 2)
     (floor (/ LIMIT x))
     (+ 1 (floor (/ LIMIT x)))))

;; (Multiples of 3) + (Multiples of 5) - (Multiples of 3 & 5)
(- (+ (get-multiples 3) (get-multiples 5)) (get-multiples 15))