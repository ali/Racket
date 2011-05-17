;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; Project Euler - Problem #10
;; Solved by Ali Ukani on May 17, 2011
;; Calculate the sum of all the primes below two million.

(require racket)

;; divides?: Number Number -> Boolean
;; Returns true if a divides b
(define (divides? a b)
  (zero? (modulo a b)))

(check-expect (divides? 4 2) true)
(check-expect (divides? 5 8) false)

;; solve is a warapper for sum-primes: (solve limit) -> (sum-primes 2 limit empty)
;; sum-primes : Number Number [Listof Numbers]
;; Finds all the primes less than a given limit, and then returns their sum.
;; i is a counter, lim is the limit, and primes is a list of the found primes.
(define (solve limit)
  (local [(define (sum-primes i lim primes)
            (if (> i lim) (foldl + 0 primes)
                (cond
                  [(ormap (λ (p) (divides? i p)) primes) (primes (add1 i) lim primes)]
                  [else (primes (add1 i) lim (cons i primes))])))]
    (sum-primes 2 limit empty)))
;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
(check-expect (solve 10) 17)

;; Solve
(time (solve 2000000))