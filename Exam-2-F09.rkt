;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exam-2-F09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; =========
;; Problem 1
;; =========

(define a '(1 2))
(define b '((3 4) (5 6)))

;; 1
(check-expect (append a b)
              '(1 2 (3 4) (5 6)))

;; 2
(check-expect (list a b)
              (list (list 1 2)
                    (list (list 3 4)
                          (list 5 6))))

;; 3
(check-expect (cons a b)
              (list (list 1 2)
                    (list 3 4)
                    (list 5 6)))

;; 4
(check-expect (apply append a b)
              '(1 2 3 4 5 6))

;; =========
;; Problem 2
;; =========

;;; A [Setof X] is a [Listof X].
;;; Repetitions not allowed.

;;; abs=? : Number Number -> Boolean
;;; Do the two numbers have the same absolute value?
(define (abs=? x y) (= (abs x) (abs y)))
(check-expect (abs=? 0 0) true)
(check-expect (abs=? 3 3) true)
(check-expect (abs=? 3 -3) true)
(check-expect (abs=? -3 3) true)
(check-expect (abs=? -3 -3) true)
(check-expect (abs=? 3 0) false)

;; contains?: [Setof X] X (X X -> Boolean) -> Boolean
;; Checks if a given set contains a given element.
(define (contains? s item elt=?)
  (ormap (λ (x) (elt=? x item)) s))

(check-expect (contains? '(5 1 7) 1 abs=?) true)
(check-expect (contains? '(5 1 7) -1 abs=?) true)
(check-expect (contains? '(5 1 7) 8 abs=?) false)


;; =========
;; Problem 3
;; =========

;; andmap2500: (X -> Boolean) [Listof X] -> Boolean
;; If a given evaluation results in true for each item, function returns true.
(define (andmap2500 pred? lst)
  (cond
    [(empty? lst) true]
    [else (and (pred? (first lst))
               (andmap2500 pred? (rest lst)))]))

(check-expect (andmap2500 zero? (list 0 0 0 0 0 0 0)) true)
(check-expect (andmap2500 even? (list 2 4 6 8 10)) true)
(check-expect (andmap2500 even? (list 5 9 8 3)) false)

;; =========
;; Problem 4
;; =========

;;; An ANIMAL is (make-animal String Number)
(define-struct animal (name legs))

(define a1 (make-animal "Mr. Ed" 4))
(define a2 (make-animal "Flipper" 0))
(define a3 (make-animal "Shelob" 8))
(define zoo (list a1 a2 a3))

;; by-legs: Number [Listof Animals] -> [Listof Animals]
;; Returns a list of animals from a given list that have a given number of legs.
(define (by-legs n lst)
  (filter (λ (a) (= (animal-legs a) n))
          lst))

(check-expect (by-legs 4 zoo) (list a1))

;; =========
;; Problem 5
;; =========
#|
(define-struct pair (a b))
;;; A [Pair X Y] is a (make-pair X Y).

;; fred: (X -> Boolean) [Listof Pairs] -> Boolean
(define (fred test lop)
  (cond [(empty? lop) false]
        [(test (pair-a (first lop))) true]
        [else (fred test (rest lop))]))
|#

;; =========
;; Problem 6
;; =========

(define-struct node (left right))

;; A [BT X] is one of:
;; - an X
;; - (make-node [BT X] [BT X])

(define bt1 (make-node (make-node "Olin"
                                  "Shivers")
                       (make-node "David"
                                  (make-node "Van"
                                             "Horn"))))

;; fold-tree: (Y Y -> Y) (X -> Y) (BT X) -> Y
;; Folds a binary-tree
(define (fold-tree combine op bt)
  (cond
    [(node? bt) (combine (fold-tree combine op (node-left bt))
                         (fold-tree combine op (node-right bt)))]
    [else (op bt)]))

(check-expect (fold-tree + string-length bt1)
              (+ (+ (string-length "Olin")
                    (string-length "Shivers"))
                 (+ (string-length "David")
                    (+ (string-length "Van")
                       (string-length "Horn")))))

;; height: [BT X] -> Number
;; Returns the height of a binary tree
(define (height bt)
  (fold-tree (λ (a b) (max (add1 a)
                           (add1 b)))
             (λ (a) (if (not (node? a)) 0 0))
             bt))

(check-expect (height bt1) 3)


;; =========
;; Problem 7
;; =========

;; snoc: X [Listof X] -> [Listof X]
;; Adds an element to the end of a list
(define (snoc x lst)
  (foldr cons (list x) lst))

(check-expect (snoc 7 '(1 3 5))
              '(1 3 5 7))

;; =========
;; Problem 8
;; =========

;;; A NumTree is a [BT Number].

;; same-shape?: [BT Number] [BT Number] -> Boolean
;; Checks if two [BT Number]s have the same node structure.
(define (same-shape? nt1 nt2)
  (cond
    [(and (node? nt1) (node? nt2)) (and (same-shape? (node-left nt1)
                                                     (node-left nt2))
                                        (same-shape? (node-right nt1)
                                                     (node-right nt2)))]
    [else (not (or (node? nt1) (node? nt2)))]))

(define ntree1 (make-node 5
                          (make-node 7 3)))
(define ntree2 (make-node 2
                          (make-node 5 8)))
(define ntree3 (make-node (make-node 5 3)
                          (make-node 8 9)))

(check-expect (same-shape? ntree1 ntree2) true)
(check-expect (same-shape? ntree1 ntree3) false)