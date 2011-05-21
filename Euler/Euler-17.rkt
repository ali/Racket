;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Euler-17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Project Euler - Problem #53
;; Solved by Ali Ukani on May 20, 2011
;; "If all the numbers from 1 to 1000 (one thousand) inclusive
;; were written out in words, how many letters would be used?"

;; I grouped numbers into sets of 1-9, 10-19, and 19-100 to take
;; care of special cases (twelve, not twoteen; fifty, not fivety)
;; and then took care of every other number.

;; This could be done better if instead of caring about written-out
;; forms of words, I skipped straight to the number of letters.
;;      (= n 1) -> 3 ("one" is three letters)
;; This way, I'd only have to map the list of numbers once to
;; return the numbers of letters each number requries to write out,
;; and then fold over the list to get a sum. This would cut out the
;; local function "count" in the "letters used" function.

;; number->word : Number -> String
;; Returns the written-out form of a given number,
;;   for any number 1 through 1000 (inclusive)
(define (number->word n)
  (cond
    [(= n 1000) "one thousand"]
    ;; Numbers 1 -> 9
    [(< n 10) (cond
                [(= n 1) "one"]
                [(= n 2) "two"]
                [(= n 3) "three"]
                [(= n 4) "four"]
                [(= n 5) "five"]
                [(= n 6) "six"]
                [(= n 7) "seven"]
                [(= n 8) "eight"]
                [(= n 9) "nine"])]
    ;; Numbers 10 -> 19
    [(< n 20) (cond
                [(= n 10) "ten"]
                [(= n 11) "eleven"]
                [(= n 12) "twelve"]
                [(= n 13) "thirteen"]
                [(= n 15) "fifteen"]
                [(= n 18) "eighteen"]
                [else (string-append (number->word (modulo n 10)) "teen")])]
    ;; Numbers 20 -> 100
    [(< 19 n 100) (cond
                 [(= n 20) "twenty"]
                 [(= n 30) "thirty"]
                 [(= n 40) "forty"]
                 [(= n 50) "fifty"]
                 [(= n 80) "eighty"]
                 ;; Sixty, Seventy, Eighty
                 [(zero? (modulo n 10)) (string-append (number->word (floor (/ n 10)))
                                                       "ty")]
                 ;; "n-ty something"
                 [else (string-append (number->word (- n (modulo n 10))) "-"
                                      (number->word (modulo n 10)))])]
    ;; "n hundred"
    [(zero? (modulo n 100)) (string-append (number->word (floor (/ n 100))) " hundred")]
    ;; "n hundred and something"
    [else (string-append (number->word (floor (/ n 100))) " hundred and "
                         (number->word (modulo n 100)))]))
(check-expect (number->word 17) "seventeen")
(check-expect (number->word 21) "twenty-one")
(check-expect (number->word 32) "thirty-two")
(check-expect (number->word 43) "forty-three")
(check-expect (number->word 54) "fifty-four")
(check-expect (number->word 65) "sixty-five")
(check-expect (number->word 76) "seventy-six")
(check-expect (number->word 87) "eighty-seven")
(check-expect (number->word 98) "ninety-eight")
(check-expect (number->word 342) "three hundred and forty-two")
(check-expect (number->word 999) "nine hundred and ninety-nine")

;; letters-used : Number -> Number
;; Given a number, counts the letters used to write it out as a word
;;   Does not count spaces or hyphens
(define (letters-used n)
  (local [(define (count lst)
            (cond
              [(empty? lst) 0]
              [(cons? lst) (if (or (string=? (first lst) "-")
                                   (string=? (first lst) " "))
                               (count (rest lst))
                               (+ 1 (count (rest lst))))]))]
    (count (explode (number->word n)))))
(check-expect (letters-used 342) 23)
(check-expect (letters-used 115) 20)

;; sum-range : Number -> Number
;; Given n, returns the sum of the letters used for each number 1 to n.
(define (sum-range n)
  (foldl + 0 (map letters-used (build-list n add1))))
(check-expect (sum-range 5) 19)

;; Solve
(time (sum-range 1000))