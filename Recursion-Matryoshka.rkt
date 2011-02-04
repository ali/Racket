;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Recursion-Matryoshka) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Recursion-Matryoshka
;;   ex from class, worked from memory
;;   02/03/2011

;; Recursion is when a function calls itself. The best way I can explain this is through an example.
;; Suppose we have a set of Russian nesting dolls (http://en.wikipedia.org/wiki/Matryoshka_doll)
;; The smallest Doll is encased by other Dolls.

;; a (make-doll Doll) is a Doll.
;; a (make-smallest) is the smallest (innermost) Doll.
;; A Doll is:
;; - a (make-doll Doll)
;; - a (make-smallest)
(define-struct doll (contents))
(define-struct smallest ())

;; Here are some examples:
(define doll-0 (make-smallest))
(define doll-1 (make-doll (make-smallest)))
(define doll-2 (make-doll (make-doll (make-smallest))))

;; A (make-smallest) has no arguments, and that's okay. It just means we won't have any accessors, but we still have the constructor ("make-smallest") and the predicate ("smallest?").

;; We have two cases for our template: either (doll-contents d) returns a Doll, or it returns a smallest. This means it's a union, so we're going to use a conditional.
;; If (doll-contents d) is a smallest, we'll figure out what to do when we write our function.
;; If (doll-contents d) is a Doll, we're going to call process-doll on that Doll.
#;(define (process-doll d)
  (cond
    [(smallest? d) ...]
    [(doll? d) (process-doll (doll-contents d))]))

;; Here's an example: we're going to count the number of layers that cover the innermost doll. We will not count the innermost doll in our total.
(define (numLayers d)
  (cond
    [(smallest? d) 0]
    [(doll? d) (+ 1 (numLayers (doll-contents d)))]))

;; Let's run some (check-expects) on our previously defined dolls.
(check-expect (numLayers doll-0) 0) ;; a (make-smallest)
(check-expect (numLayers doll-1) 1) ;; a (make-doll (make-smallest))
(check-expect (numLayers doll-2) 2) ;; a (make-doll (make-doll (make-smallest)))