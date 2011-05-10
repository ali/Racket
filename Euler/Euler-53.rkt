;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-53) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Project Euler - Problem #53
;; Solved by Ali Ukani on May 10, 2011
;; "How many values of C(n,r), for 1 ≤ n ≤ 100, exceed one-million?"

(require racket)

;; factorial : Number -> Number
;; n! = n*(n-1)*...*3*2*1, and 0! = 1.
(define (factorial n)
  (cond
    [(zero? n) 1]
    [else (* n (factorial (sub1 n)))]))
(check-expect (factorial 1) 1)
(check-expect (factorial 0) 1)
(check-expect (factorial 5) 120)

;; C : Number Number -> Number
;; n choose r
(define (C n r)
         (/ (factorial n)
            (* (factorial r) (factorial (- n r)))))
(check-expect (C 5 3) 10)

(define (euler x)
 (local [(define lst (reverse (build-list x add1)))]
           (for*/list ([n lst]
                       [r (reverse (build-list n add1))])
             (C n r))))

;; How many of the resulting combinations are >1000000?
(length (filter (λ (n) (> n 1000000)) (euler 100)))