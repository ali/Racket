;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Asteroids) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;*___________________*
;; |                  |
;; |    Asteriods     |
;; |       v3.0       |
;; |__________________|
;;*                   *
;; Ali Ukani

(require 2htdp/image)
(require 2htdp/universe)

;; Game dimensions
(define WIDTH 600)
(define HEIGHT 400)

;; Bullet age (how many ticks a bullet lasts)
(define BULLET-AGE 30)
;; Initial bullet speed
(define INIT-BULL-SPEED 5)

;; Initial ship speed
(define INIT-SHIP-SPEED 0)

;; Initial number of astroids
(define INIT-NUM-ASTEROIDS 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model

;; A Ship is a (make-ship Posn Number Number)
(define-struct ship (posn speed angle))
;; where posn is the position of the ship on the canvas,
;;   speed is the ship's speed and angle is the angle
;;   of the ship (in degrees)

;; An Asteroid is a (make-asteroid Posn Posn Symbol)
(define-struct asteroid (posn vel size))
;; where posn is the position of the asteroid on the canvas
;;        vel is the x/y velocity of the asteroid 
;;   and size is the size (image) of the asteroid (either 'large or 'small)

;; A Bullet is (make-bullet Posn Posn Integer)
(define-struct bullet (posn vel age))
;; where posn is the position of the bullet on the canvas,
;;   vel is the x/y velocity of the bullet
;;   and age is how many ticks the bullet has left

;; A World is a (make-world Ship ListOfAsteroids ListOfBullets Number Number Number)
(define-struct world (ship asters bulls lives level score))

;; A ListOfAsteroids (LoA) is one of:
;; - empty
;; - (cons Asteroid LoA)

;; A ListOfBullets (LoB) is one of:
;; - empty
;; - (cons Bullet LoB)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controller

;; angle->posn: Number => Posn
;; Takes an angle in degrees and returns a directional Posn
;;   Accounts for the downward-increasing Y-axis of scenes/images
;;   by multiplying the posn-y by -1
(define (angle->posn a)
  (make-posn (inexact->exact (round (cos (* a pi 1/180))))
             (* -1 (inexact->exact (round (sin (* a pi 1/180)))))))
(check-expect (angle->posn 0)   (make-posn  1  0))
(check-expect (angle->posn 45)  (make-posn  1  -1))
(check-expect (angle->posn 90)  (make-posn  0  -1))

;; add-posns: Posn Posn => Posn
;; Adds two Posns together.
(define (add-posns a b)
  (make-posn (modulo (+ (posn-x a) (posn-x b)) WIDTH)
             (modulo (+ (posn-y a) (posn-y b)) HEIGHT)))
(check-expect (add-posns (make-posn 1 1) (make-posn 1 1)) (make-posn 2 2))

;; posn*: Posn Number => Posn
;; Multiplies a Posn by a given number.
(define (posn* p n)
  (make-posn (modulo (* (posn-x p) n) WIDTH)
             (modulo (* (posn-y p) n) HEIGHT)))
(check-expect (posn* (make-posn 1 1) 3) (make-posn 3 3))

;; posn=?: Posn Posn => Boolean
;; Checks if two posns are equal.
(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

;; turn: Number World => World
;; Takes a world and rotates its ship.
(define (turn deg w)
  (make-world (make-ship (ship-posn (world-ship w))
                         (ship-speed (world-ship w))
                         (modulo (+ deg (ship-angle (world-ship w))) 360))
              (world-asters w)
              (world-bulls w)
              (world-lives w)
              (world-level w)
              (world-score w)))

;; boost-ship: World => World
;; Takes a world and increases the ship's speed
(define (boost-ship w)
  (make-world (make-ship (ship-posn (world-ship w))
                         (+ 7 (ship-speed (world-ship w)))
                         (ship-angle (world-ship w)))
              (world-asters w)
              (world-bulls w)
              (world-lives w)
              (world-level w)
              (world-score w)))

;; move-ship: Ship => Ship
;; Moves a ship forward in the direction
;;   it is facing by a factor of its speed
(define (move-ship s)
  (make-ship (add-posns (ship-posn s)
                        (posn* (angle->posn (ship-angle s))
                               (ship-speed s)))
             (if (zero? (ship-speed s)) 0 (- (ship-speed s) 1))
             (ship-angle s)))

;; make-asteroids: Natural Symbol => [Listof Asteroids]
;; Returns a randomly generated list (of a given length) of Asteroids of a given size.
(define (make-asteroids num)
  (cond
    [(zero? num) empty]
    [else (cons (make-asteroid (make-posn (random WIDTH)
                                          (random HEIGHT))
                               (make-posn (- (random 8) 4)
                                          (- (random 8) 4))
                               'large)
                (make-asteroids (- num 1)))]))

;; move-asteroids: [Listof Asteroids] => [Listof Asteroids]
;; Takes a [Listof Asteroids] and moves all of the asteroids.
(define (move-asteroids lst)
  (local [(define (move-roid a)
            (make-asteroid (add-posns (asteroid-posn a)
                                      (asteroid-vel a))
                           (asteroid-vel a)
                           (asteroid-size a)))]
    (map move-roid lst)))

;; large?: Asteroid -> Boolean
;; Returns true if an asteroid is large
(define (large? a)
  (symbol=? (asteroid-size a) 'large))
(check-expect (large? (make-asteroid (make-posn 1 1) (make-posn 1 1) 'large)) true)
(check-expect (large? (make-asteroid (make-posn 1 1) (make-posn 1 1) 'small)) false)

;; small?: Asteroid -> Boolean
;; Returns true if an asteroid is small
(define (small? a)
  (symbol=? (asteroid-size a) 'small))
(check-expect (small? (make-asteroid (make-posn 1 1) (make-posn 1 1) 'small)) true)
(check-expect (small? (make-asteroid (make-posn 1 1) (make-posn 1 1) 'large)) false)

;; hit?: Asteroid Bullet -> Boolean
;; Checks if an Asteroid and a Bullet occupy the same space.
(define (hit? a b)
  (local [(define A-WIDTH  (cond [(large? a) (* 0.5 (image-width  LARGE-ASTEROID-IMAGE))]
                                 [(small? a) (* 0.5 (image-width  SMALL-ASTEROID-IMAGE))]))
          (define A-HEIGHT (cond [(large? a) (* 0.5 (image-height LARGE-ASTEROID-IMAGE))]
                                 [(small? a) (* 0.5 (image-height SMALL-ASTEROID-IMAGE))]))]
    (and (< (- (posn-x (asteroid-posn a)) A-WIDTH)
            (posn-x (bullet-posn b))
            (+ (posn-x (asteroid-posn a)) A-WIDTH))
         (< (- (posn-y (asteroid-posn a)) A-HEIGHT)
            (posn-y (bullet-posn b))
            (+ (posn-y (asteroid-posn a)) A-HEIGHT)))))

;; filter-small-asteroids: [Listof Asteroids] [Listof Bullets] => [Listof Asteroids]
;; Filters out all small asteroids that have been hit by bullets.
(define (filter-small-asteroids loa lob)
  (local [(define (not-hit? a) (not (ormap (lambda (b) (hit? a b))
                                           lob)))
          (define (keep? a) (cond [(large? a) true]
                                  [(small? a) (not-hit? a)]))]
    (filter keep? loa)))

;; filter-large-asteroids: [Listof Asteroids] [Listof Bullets] => [Listof Asteroids]
;; Breaks apart any large asteroids that have been hit by bullets into small asteroids.
(define (filter-large-asteroids loa lob)
  (local [(define (make-smalls posn num) (cond [(zero? num) empty]
                                               [else (cons (make-asteroid posn
                                                                          (make-posn (- (random 8) 4)
                                                                                     (- (random 8) 4))
                                                                          'small)
                                                           (make-smalls posn (- num 1)))]))
          (define (break? a) (and (large? a)
                                  (ormap (lambda (b) (hit? a b)) lob)))
          (define (break-if loa) (cond
                                     [(empty? loa) empty]
                                     [(cons? loa) (cond
                                                    [(break? (first loa)) (append (make-smalls (asteroid-posn (first loa)) 3)
                                                                                  (break-if (rest loa)))]
                                                    [(not (break? (first loa))) (cons (first loa)
                                                                                      (break-if (rest loa)))])]))]
    (break-if loa)))

;; filter-asteroids: [Listof Asteroids] [Listof Bullets] -> [Listof Asteroids]
;; Breaks apart large asteroids and destroys small asteroids that have been hit.
(define (filter-asteroids loa lob)
  (filter-large-asteroids (filter-small-asteroids loa lob) lob))
            
;; shoot: World => World
;; Adds a new bullet to a World
(define (shoot w)
  (make-world (world-ship w)
              (world-asters w)
              (cons (make-bullet (ship-posn (world-ship w))
                                 (posn* (angle->posn (ship-angle (world-ship w))) INIT-BULL-SPEED)
                                 BULLET-AGE)
                    (world-bulls w))
              (world-lives w)
              (world-level w)
              (world-score w)))

;; move-bullets: [Listof Bullets] => [Listof Bullets]
;; Takes a [Listof Bullets]. Moves every bullet and reduces its age.
(define (move-bullets lst)
  (local [(define (move-bullet b)
            (make-bullet (add-posns (bullet-posn b)
                                    (bullet-vel b))
                         (bullet-vel b)
                         (- (bullet-age b) 1)))]
    (map move-bullet lst)))

;; filter-hit-bullets: [Listof Bullets] [Listof Asteroids] -> [Listof Bullets]
;; Filters out any bullets that have hit an asteroid.
(define (filter-hit-bullets lob loa)
  (local [(define (has-not-hit? b) (not (ormap (lambda (a) (hit? a b)) loa)))]
    (filter has-not-hit? lob)))

;; filter-dead-bullets: [Listof Bullets] => [Listof Bullets]
;; Filters out any dead (age=0) bullets from a [Listof Bullets]
(define (filter-dead-bullets lst)
  (filter (λ (b) (> (bullet-age b) 0)) lst))

;; award-points: [Listof Asteroids] [Listof Asteroids] -> Number
;; Calculates the amount of points awarded.
;; Points for each asteroid destroyed:
;;   - large: 5 points
;;   - small: 10 points
;; With n Asteroids, max points = (n * (5 + 3*10))
(define (award-points lst-1 lst-2 lst-3)
  (local [(define (count-roids size lst) (foldl (λ (a b) (if (symbol=? size (asteroid-size a))
                                                             (add1 b)
                                                             b))
                                                0
                                                lst))]
  (+ (* 5  (- (count-roids 'large lst-1)
              (count-roids 'large lst-2)))
     (* 10 (- (count-roids 'small lst-2)
              (count-roids 'small lst-3))))))

;; check-lives: World -> Number
;; If the ship has collided with an asteroid, lose a life and return to center.
(define (check-lives w)
  (local [;; Turn the ship into a bullet so we can use (hit? Asteroid Bullet)
          (define b (make-bullet (ship-posn (world-ship w))
                                 (make-posn 0 0)
                                 100))]
    (if (ormap (lambda (a) (hit? a b)) (world-asters w))
        (make-world (make-ship (make-posn (* WIDTH 0.5)
                                          (* HEIGHT 0.6))
                               INIT-SHIP-SPEED
                               90)
                           (make-asteroids INIT-NUM-ASTEROIDS)
                           empty
                           (sub1 (world-lives w))
                           (world-level w)
                           (- (world-score w) 20))
        w)))

;; tock: World -> World
;; Takes a world, moves the ship, and moves+filters bullets and asteroids.
(define (tock w)
  (local [(define LoA-1 (world-asters w))
          (define LoB-1 (filter-dead-bullets (world-bulls w)))

          ;; Break apart large asteroids that have been hit by bullets
          (define LoA-2 (filter-large-asteroids LoA-1 LoB-1))
          ;; Remove bullets that hit large asteroids
          (define LoB-2 (filter-hit-bullets LoB-1 (filter large? LoA-1)))
          
          ;; Filter out small asteroids that have been hit by bullets
          (define LoA-3 (filter-small-asteroids LoA-2 LoB-2))
          ;; Remove bullets that hit small asteroids
          (define LoB-3 (filter-hit-bullets LoB-2 (filter small? LoA-3)))
          
          ;; Move asteroids and bullets
          (define asteroids (move-asteroids LoA-3))
          (define bullets (move-bullets LoB-3))
          
          (define score (+ (world-score w)
                           (award-points LoA-1 LoA-2 LoA-3)))
          ]
    (check-lives (make-world (move-ship (world-ship w))
                             asteroids
                             bullets
                             (world-lives w)
                             (world-level w)
                             score))))
  

;; key-handler: World KeyEvent => World
;; User controls:
;; - up     move forward     alt: w
;; - left   turn left        alt: a
;; - right  turn right       alt: d
;; - space  fire
(define (key-handler w k)
  (cond
    [(or (key=? k "w") (key=? k "up")) (boost-ship w)]
    [(or (key=? k "a") (key=? k "left"))  (turn  45 w)]
    [(or (key=? k "d") (key=? k "right")) (turn -45 w)]
    [(key=? k " ") (shoot w)]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View

(define SPACE (rectangle WIDTH HEIGHT "solid" "black"))

;; The Ship: A triangle with a dip in the back.
(define SHIP-IMAGE
  (overlay (polygon (list (make-posn 0 0) (make-posn -25 10)
                          (make-posn -18 0) (make-posn -25 -10))
                    "outline"
                    "white")
           (circle 16 "outline" "black")))

;; Large-sized asteroid.
(define LARGE-ASTEROID-IMAGE
  (polygon (list (make-posn 0 0) (make-posn 18 8) (make-posn 25 17)
                 (make-posn 30 30) (make-posn 22 34) (make-posn 4 42)
                 (make-posn -11 38) (make-posn -23 27) (make-posn -18 17)
                 (make-posn -7 7))
           "outline"
           "white"))

;; Small-sized asteroid.
(define SMALL-ASTEROID-IMAGE
  (scale 0.5 LARGE-ASTEROID-IMAGE))

;; print-text: String Number => Image
;; Outputs an image of a given string in a given font size
(define (print-text s n)
  (text/font s
             n
             "white" 
             "Consolas"
             'system
             'normal
             'normal
             #f))

;; place-ship : Ship Scene => Scene
;; Places a ship onto a scene.
(define (place-ship ship scene)
  (place-image (rotate (ship-angle ship) SHIP-IMAGE)
               (posn-x (ship-posn ship))
               (posn-y (ship-posn ship))
               scene))

;; place-asteroids : [Listof Asteroids] Scene => Scene
;; Takes a [Listof Asteroids] and places it on a given Scene
(define (place-asteroids lst scene)
  (local [(define (place-roid a b)
            (place-image (cond [(large? a) LARGE-ASTEROID-IMAGE]
                               [(small? a) SMALL-ASTEROID-IMAGE])
                         (posn-x (asteroid-posn a))
                         (posn-y (asteroid-posn a))
                         b))]
    (foldr place-roid scene lst)))

;; place-bullets : [Listof Bullets] Scene => Scene
;; Takes a [Listof Bullets] and places it on a given Scene
(define (place-bullets lst scene)
  (local [(define (place-bullet a b)
            (place-image (square 3 "solid" "white")
                          (posn-x (bullet-posn a))
                          (posn-y (bullet-posn a))
                          b))]
    (foldr place-bullet scene lst)))

;; show-HUD: World Scene -> Scene
;; Displays the score and amount of lives a user has
(define (show-HUD w sc)
  (local [(define the-HUD (print-text (string-append "Score: " (number->string (world-score w)) "  "
                                                     "Lives: " (number->string (world-lives w)))
                                      15))]
    (place-image the-HUD
                 (/ WIDTH 2) (round (* 0.8 (image-height the-HUD)))
                 sc)))

;; display: World -> Scene
;; Draws the world
(define (display w)
  (show-HUD w
            (place-bullets (world-bulls w)
                           (place-asteroids (world-asters w)
                                            (place-ship (world-ship w)
                                                          SPACE)))))

;; END-SCREEN: World -> Scene
;; End screen. Displays a message and the score.
(define (END-SCREEN w)
  (local [(define str
            (cond [(zero? (world-lives w)) "YOU LOSE"]
                  [(zero? (length (world-asters w))) "YOU WIN"]))]
  (overlay (above (print-text str 36)
                             (print-text (string-append "Score: "
                                                        (number->string (world-score w)))
                                         24))
                             SPACE)))


;; game-end?: World -> Boolean
;; Returns true if there are no asteroids or lives left.
(define (game-end? w)
  (or (zero? (world-lives w))
      (zero? (length (world-asters w)))))

;; Starting world
(define world0 (make-world (make-ship (make-posn (* WIDTH 0.5)
                                                 (* HEIGHT 0.6))
                                      INIT-SHIP-SPEED
                                      90)
                           (make-asteroids INIT-NUM-ASTEROIDS)
                           empty
                           5
                           1
                           0))

;; big-bang
(big-bang world0
            (on-tick tock)
            (on-key key-handler)
            (on-draw display)
            (stop-when game-end? END-SCREEN)
            (state false))