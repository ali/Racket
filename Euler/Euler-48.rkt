;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-48) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #48
;; Solved by Ali Ukani on May 19, 2011
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

;; What λ does:
;; ex: ((λ (i) (+ i 2)) 3)
;; For a given i, add 2. 3 is given to the λ function, and the output is 5.
;; It helps to read the comments on the following code backwards:

((λ (sum) (modulo sum (expt 10 10))) ;; ...and then get the last 10 digits
 ;; ...compute the sum of each number raised to itself...
 ((λ (lst) (foldl (λ (a b) (+ (expt a a) b)) 0 lst))
  (build-list 1000 add1))) ;; For each number in the series...