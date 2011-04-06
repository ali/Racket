;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Dragon Factal implemented in Racket (http://racket-lang.org)
;; Ali Ukani - 03/30/2011

(require 2htdp/image)
(require 2htdp/universe)

;; A direction is one of:
;; - 'left
;; - 'right
;; - 'up
;; - 'down

#;(define (process-dir dir)
  (cond
    [(symbol=? dir 'up) ...]
    [(symbol=? dir 'down) ...]
    [(symbol=? dir 'right) ...]
    [(symbol=? dir 'left) ...]))
    

;; rotate-dir : Dir -> Dir
;; Rotate the given direction to the 'left' (counter-clockwise)
(define (rotate-dir dir)
  (cond
    [(symbol=? dir 'left) 'down]
    [(symbol=? dir 'down) 'right]
    [(symbol=? dir 'right) 'up]
    [(symbol=? dir 'up) 'left]))

;; rotate-dirs : [Listof Dir] -> [Listof Dir]
;; Rotates all the Dirs in a [Listof Dir] counter-clockwise
(define (rotate-dirs lst)
  (map rotate-dir lst))

;; move-posn : Number Number Symbol Number -> Posn
;; Returns a Posn that is the result of moving the given x and y in the given Direction, the given amount, amt.
(define (move-posn x y dir amt)
  (cond
    [(symbol=? dir 'up)    (make-posn x (- y amt))]
    [(symbol=? dir 'down)  (make-posn x (+ y amt))]
    [(symbol=? dir 'left)  (make-posn (- x amt) y)]
    [(symbol=? dir 'right) (make-posn (+ x amt) y)]))
(check-expect (move-posn 100 100 'up 50) (make-posn 100 50))
(check-expect (move-posn 100 100 'down 50) (make-posn 100 150))
(check-expect (move-posn 100 100 'left 50) (make-posn 50 100))
(check-expect (move-posn 100 100 'right 50) (make-posn 150 100))

;; draw-line: Posn Posn Color Scene => Scene
;; This is scene+line using Posns instead of Numbers
(define (draw-line posn1 posn2 color image)
  (scene+line image
              (posn-x posn1)
              (posn-y posn1)
              (posn-x posn2)
              (posn-y posn2)
              color))

;; draw-dirs : [Listof Dir] Number Number Color Scene -> Scene
;; Draw lines of given color, following the given directions starting at (x,y) into
;;   the given Scene.
(define (draw-dirs lst x y color scene)
  (cond
    [(empty? lst) scene]
    [(cons? lst) (local [(define next-posn (move-posn x y (first lst) 3))]
                   (draw-dirs (rest lst)
                            (posn-x next-posn)
                            (posn-y next-posn)
                            color
                            (draw-line (make-posn x y)
                                       next-posn
                                       color
                                       scene)))]))

;; Screen Size...
(define W 400)
(define H 400)

#| This is old

;; Draw wrapper
(define (draw w)
  (local ((define lst (reverse w)))
    (draw-dirs lst (/ W 2) (/ H 2) "red" (empty-scene W H))))

;; Key Handler
(define (key w ke)
  (cond [(key=? ke "up") (cons 'up w)]
        [(key=? ke "down") (cons 'down w)]
        [(key=? ke "left") (cons 'left w)]
        [(key=? ke "right") (cons 'right w)]
        [(key=? ke "r") (rotate-dirs w)]
        [else w]))

(big-bang '()
          (on-draw draw)
          (on-key key))
|#

;; jurassic: [Listof Dir] Number -> [Listof Dir]
;; Compute the next iteration of the Jurassic Fractal, given a [Listof Dir]
;;   and the number of iterations left.
(define (jurassic lod iter)
  (cond
    [(zero? iter) lod]
    [else (jurassic (append lod (reverse (rotate-dirs lod)))
                    (sub1 iter))]))

(define (draw w)
  (local [(define lst1 (jurassic '(down) w))
          (define lst2 (rotate-dirs lst1))
          (define lst3 (rotate-dirs lst2))
          (define lst4 (rotate-dirs lst3))
          (define (draw-one lst color scn)
            (draw-dirs lst (/ W 2) (/ H 2) color scn))]
    (draw-one lst1 "red"
              (draw-one lst2 "green"
                        (draw-one lst3 "blue"
                                  (draw-one lst4 "black" (empty-scene W H)))))))

(define (key w ke)
  (cond [(key=? ke "up") (add1 w)]
        [(and (key=? ke "down") (> w 1))
         (sub1 w)]
        [else w]))

(big-bang 0
          (on-draw draw)
          (on-key key))