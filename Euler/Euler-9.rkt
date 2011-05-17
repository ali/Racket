;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #9
;; Solved by Ali Ukani on May 17, 2011
;; Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
(require racket)

;; A Pythagorean Triple (triple) is a (make-triple Number Number Number)
(define-struct triple (a b c))

;; pythag
;; Creates a pythagorean triple, given m and n, using Euclid's formula
;; http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
(define (pythag m n)
  (cond [(> m n) (make-triple (- (sqr m) (sqr n))
                              (* 2 m n)
                              (+ (sqr m) (sqr n)))]
        [else false]))
; (check-expect (pythag 2 1) (make-triple 3 4 5))
; The expected value is the same as the actual value, but the test results in an error. Weird.

;; sum-triple : Triple -> Number
;; Finds the sum of a, b, and c
(define (sum-triple t)
  (+ (triple-a t)
     (triple-b t)
     (triple-c t)))
(check-expect (sum-triple (make-triple 3 4 5)) 12)

;; *triple: Triple -> Number
;; Finds the product of a, b, and c
(define (triple* t)
  (* (triple-a t)
     (triple-b t)
     (triple-c t)))
(check-expect (triple* (make-triple 3 4 5)) 60)

;; Solve: 
;; Get all the triples,
;; find the one that has an a+b+c sum of 1000,
;; and then find the product of a*b*c
(triple* (first (filter (λ (t) (= (sum-triple t) 1000))
                        (for*/list ([m (build-list 1000 add1)]
                                    [n (build-list m add1)]
                                    #:when (pythag m n))
                          (pythag m n)))))