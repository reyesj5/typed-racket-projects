#lang typed/racket

;; CMSC15100 Winter 2017
;; Homework 1
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; Problem #1
;;
(: eval-quadratic : Real Real Real Real -> Real)
;; evaluates a quadratic function from the arguments at a specified x
;;
(define (eval-quadratic a b c x)
  (+ (* a (* x x)) (* b x) c))
(check-expect (eval-quadratic 1 2 -3 -1) -4)
(check-within (eval-quadratic 3 -5 +10 .5) 8.25 0.001)
(check-expect (eval-quadratic 3 -5 +10 6) 88)

(: within? : Real Real Real -> Boolean)
;; tests if two numbers are within a given epsilon of each other
;;
(define (within? ε x1 x2)
  (cond
    [(and (< x1 x2) (< (- x2 x1) ε)) #t]
    [(and (< x2 x1) (< (- x1 x2) ε)) #t]
    [else #f]))
(check-expect (within? 2 3 4) #t)
(check-expect (within? .2 .215 .6120) #f)
(check-expect (within? .0005 -.254651 -.2549854) #t)
(check-expect (within? 2 -4 -3) #t)

(: on-quadratic : Real Real Real Real Real Real -> Boolean)
;; determines if two points within epsilon of eachother from given values
;;
(define (on-quadratic a b c x y ε)
  (: f-x : Real)
  ;; calls eval-quadratic to determine the value of f(x)
  ;;
  (define f-x (eval-quadratic a b c x))
  (if (within? ε y f-x) #t #f))
(check-expect (on-quadratic 5 -3 2.5 1.2 6.095 .01) #t)
(check-expect (on-quadratic 3 6 -5 -.3 -6.535694 .006) #t)
(check-expect (on-quadratic 3 6 -5 -3.6 15 .1) #f)

;; Problem #2
;;
(: sq : Real -> Real)
;; squares a real number
;;
(define (sq a) (* a a))
(check-expect (sq 8) 64)
(check-expect (sq -4) 16)
(check-expect (sq 12) 144)

(: pythagorean? : Integer Integer Integer -> Boolean)
;; checks if the given arguments are a Pythagorean tripple
;;
(define (pythagorean? a b c)
  (if (= (+ (sq a) (sq b)) (sq c)) #t #f))
(check-expect (pythagorean? 3 4 5) #t)
(check-expect (pythagorean? 3 4 6) #f)
(check-expect (pythagorean? 5 12 13) #t)

;; Problem #3
;;
(: valid-board? : Integer Integer Integer -> Boolean)
;; checks if the given arguments allow for a possible board in the game
;;
(define (valid-board? bS nX nO)
  (cond
    [(or (< nX 0) (< nX nO) (< (+ nO 1) nX)) #f]
    [(<= (+ nX nO) (sq bS)) #t]
    [else #f]))
(check-expect (valid-board? 5 7 8) #f)
(check-expect (valid-board? 3 5 4) #t)
(check-expect (valid-board? 4 7 7) #t)
(check-expect (valid-board? 6 19 18) #f)
(check-expect (valid-board? 2 0 0) #t)
(check-expect (valid-board? 9 10 7) #f)

;; Problem #4
;;
(: income-tax : Integer -> Integer)
;; calculates the amount of tax owed based on the British income-tax system
;;
(define (income-tax i)
  (cond
    [(< i 11001) 0]
    [(< i 43001) (exact-ceiling (* .2 (- i 11000)))]
    [(< i 150001) (exact-ceiling (+ (* .4 (- i 43000)) (income-tax 43000)))]
    [else (exact-ceiling (+ (* .45 (- i 150000)) (income-tax 150000)))]))
(check-expect (income-tax 9002) 0)
(check-expect (income-tax 25645) 2929)
(check-expect (income-tax 43001) 6401)
(check-expect (income-tax 954621) 411280)

;; Problem #5
;;
(: leo : Integer -> Integer)
;; returns the Leonardo number at the given n position (counting zero) 
;;
(define (leo n)
  (cond
    [(< n 0) 0]
    [(or (= n 0) (= n 1)) 1]
    [else (+ 1 (leo (- n 1)) (leo (- n 2)))]))
(check-expect (leo -5) 0)
(check-expect (leo 5) 15)
(check-expect (leo 3) 5)
;; run tests
;;
(test)