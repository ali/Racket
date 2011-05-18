;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #14
;; Solved by Ali Ukani on May 18, 2011
;; "Which starting number, under one million, produces the longest chain?"

(define (f n)
  (cond
    [(= (modulo n 2) 0) (/ n 2)]
    [else (+ (* 3 n) 1)]))
(check-expect (f 13) 40)
(check-expect (f 40) 20)
(check-expect (f 5) 16)

(define (chain i)
  (local [(define (loop i lst)
  (cond
    [(= i 1) (append lst '(1))]
    [else (loop (f i)
                 (append lst (list i)))]))]
    (loop i empty)))
(check-expect (chain 13) '(13 40 20 10 5 16 8 4 2 1))

(define (loop i lst)
  (cond
    [(empty? lst) i]
    [(cons? lst) ((λ (j) (cond
                          [(< (first (cdr i)) (first (cdr j))) (loop j (rest lst))]
                          [else (loop i (rest lst))]))
                  (list (first lst) (length (chain (first lst)))))]))
(check-expect (loop '(0 0) (build-list 5 add1)) '(3 8))

;; Solve
(define (Euler n)
  (loop '(0 0) (build-list n add1)))

(Euler 999999)