;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Project Euler - Problem #4 
;; Solved by Ali Ukani on May 5, 2011
;; "Find the largest palindrome made from the product of two 3-digit numbers."

(require racket)

;; List of numbers from 100 to 999
(define LST (build-list 900 (λ (n) (+ n 100))))
(check-expect (andmap (λ (n) (< 99 n 1000)) LST) true)

;; is-palindrome? : Number -> Boolean
;; Returns true if a number is a palindrome
(define (palindrome? n)
  (local [(define (flip n)
            (string->number (implode (reverse (explode (number->string n))))))]
  (= (flip n) n)))
(check-expect (palindrome? 1) true)
(check-expect (palindrome? 10005) false)
(check-expect (palindrome? 90009) true)

;; Finds the products of every number from 100 to 999
;; (999*999, 999*998, ... 998*999, 998*998, 998*997, ...)
;; and creates a list of the products that are palindromes.
;; Then find the max number by folding over the list.
(foldl max 0 (for*/list ([i LST]
                         [j LST]
                         #:when (palindrome? (* i j)))
               (* i j)))