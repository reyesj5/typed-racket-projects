#lang typed/racket

;; CMSC15100 Winter 2017
;; Homework 2
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; The days of the week
(define-type Weekday
  (U 'Sunday 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday))

;; Representation of dates
(define-struct Date
  ([month : Integer]    ;; value from 1..12 that represents January, ...
   [day : Integer]      ;; value from 1..31 that represents the day of the month
   [year : Integer]))   ;; value from 1900..2199 that represents the year

;; Problem 1
;;
(: leap-year? : Integer -> Boolean)
;; Checks if argument is a leap-year
;;
(define (leap-year? year)
  (cond
    [(or (and (= (remainder year 4) 0) (not (= (remainder year 100) 0)))
         (= (remainder year 400) 0)) #t]
    [else #f]))
(check-expect (leap-year? 1904) #t)
(check-expect (leap-year? 1903) #f)
(check-expect (leap-year? 2020) #t)

(: days-in-month : Integer Integer -> Integer)
;; returns the number of days in each month for that year
;;
(define (days-in-month month year)
  (cond
    [(or (= month 4) (= month 6) (= month 9) (= month 11)) 30]
    [(and (leap-year? year) (= month 2)) 29] 
    [(= month 2) 28]
    [else 31]))
(check-expect (days-in-month 2 2011) 28)
(check-expect (days-in-month 2 2012) 29)
(check-expect (days-in-month 5 2012) 31)
(check-expect (days-in-month 6 2013) 30)
(check-expect (days-in-month 9 2012) 30)

(: valid-date? : Date -> Boolean)
;; Determines if the argument is a valid date
;;
(define (valid-date? date)
  (local
    {(define day : Integer (Date-day date))
     (define month : Integer (Date-month date))
     (define year : Integer (Date-year date))}
    (cond
      [(and (<= 1 month 12)
            (<= 1900 year 2099)
            (cond
              [(<= 1 day 28) #t]
              [(and  (or (= month 4) (= month 6) (= month 9) (= month 11)) (not (<= 1 day 30))) #f]
              [(and (not (leap-year? year)) (= month 2) (not (<= 1 day 28))) #f] 
              [(<= 1 day 31) #t]
              [else #f])) #t]
      [else #f])))
(check-expect (valid-date? (Date 11 30 1997)) #t)
(check-expect (valid-date? (Date 2 29 2004)) #t)
(check-expect (valid-date? (Date 5 31 1905)) #t)
(check-expect (valid-date? (Date -1 8 2000)) #f)
(check-expect (valid-date? (Date 2 29 2001)) #f)
(check-expect (valid-date? (Date 5 0 2000)) #f)

(: get-adj : Integer Integer -> Integer)
;; returns the adjustment adj value for a given date
;;
(define (get-adj month year)
  (cond
    [(or (and (leap-year? year) (= month 1)) (= month 4) (= month 7)) 0]
    [(or (= month 1) (= month 10)) 1]
    [(= month 5) 2]
    [(or (and (leap-year? year) (= month 2)) (= month 8)) 3]
    [(or (= month 3) (= month 11)) 4]
    [(= month 6) 5]
    [else 6]))
(check-expect (get-adj 1 2000) 0)
(check-expect (get-adj 9 2003) 6)
(check-expect (get-adj 8 2003) 3)

(: day-of-week : Date -> Weekday)
;; returns the string name of the week day fom given date arguments
;;
(define (day-of-week date)
  (local
    {(define day : Integer (Date-day date))
     (define month : Integer (Date-month date))
     (define year : Integer (Date-year date))
     (define n : Integer (+ (- year 1900) (get-adj month year) day (exact-floor (/ year 4))))
     (define w : Integer (remainder n 7))}
    (cond
      [(= w 1) 'Monday]
      [(= w 2) 'Tuesday]
      [(= w 3) 'Wednesday]
      [(= w 4) 'Thursday]
      [(= w 5) 'Friday]
      [(= w 6) 'Saturday]
      [else 'Sunday])))
(check-expect (day-of-week (Date 1 11 2017)) 'Wednesday)
(check-expect (day-of-week (Date 2 29 1996)) 'Thursday)
(check-expect (day-of-week (Date 3 15 2013)) 'Friday)
(check-expect (day-of-week (Date 4 27 2013)) 'Saturday)
(check-expect (day-of-week (Date 5 5 2013)) 'Sunday)
(check-expect (day-of-week (Date 6 10 2013)) 'Monday)
(check-expect (day-of-week (Date 7 23 2013)) 'Tuesday)
(check-expect (day-of-week (Date 8 22 2013)) 'Thursday)
(check-expect (day-of-week (Date 9 18 2013)) 'Wednesday)
(check-expect (day-of-week (Date 10 25 2013)) 'Friday)
(check-expect (day-of-week (Date 11 3 2013)) 'Sunday)
(check-expect (day-of-week (Date 12 19 2013)) 'Thursday)

(: date<? : Date Date -> Boolean)
;; checks if date1 is less than date2
;;
(define (date<? date1  date2)
  (local
    {(define day1 : Integer (Date-day date1))
     (define month1 : Integer (Date-month date1))
     (define year1 : Integer (Date-year date1))
     (define day2 : Integer (Date-day date2))
     (define month2 : Integer (Date-month date2))
     (define year2 : Integer (Date-year date2))}
  (cond
    [(< year1 year2) #t]
    [(and (= year1 year2) (< month1 month2)) #t]
    [(and (= year1 year2) (= month1 month2) (< day1 day2)) #t]
    [else #f])))
(check-expect (date<? (Date 9 18 2013) (Date 10 18 2013)) #t)
(check-expect (date<? (Date 10 25 2012) (Date 10 25 2013)) #t)
(check-expect (date<? (Date 11 3 2013) (Date 11 18 2013)) #t)
(check-expect (date<? (Date 12 19 2013) (Date 12 19 2013)) #f)
(check-expect (date<? (Date 9 18 2013) (Date 10 18 2003)) #f)
(check-expect (date<? (Date 10 25 2013) (Date 10 2 2013)) #f)
(check-expect (date<? (Date 11 3 2013) (Date 3 18 2013)) #f)

(: date=? : Date Date -> Boolean)
;; checks if the given dates are equal
;;
(define (date=? date1 date2)
  (and (= (Date-year date1) (Date-year date2))
       (= (Date-month date1) (Date-month date2))
       (= (Date-day date1) (Date-day date2))))
(check-expect (date=? (Date 9 18 2013) (Date 10 18 2013)) #f)
(check-expect (date=? (Date 10 25 2012) (Date 10 25 2013)) #f)
(check-expect (date=? (Date 11 3 2013) (Date 11 18 2013)) #f)
(check-expect (date=? (Date 12 19 2013) (Date 12 19 2013)) #t)
(check-expect (date=? (Date 9 18 2013) (Date 10 18 2003)) #f)
(check-expect (date=? (Date 10 25 2013) (Date 10 2 2013)) #f)
(check-expect (date=? (Date 11 3 2013) (Date 3 18 2013)) #f)

(: date>? : Date Date -> Boolean)
;; checks if date1 is greater than date2
;;
(define (date>? date1 date2)
  (and (not (date<? date1 date2)) (not (date=? date1 date2))))
(check-expect (date>? (Date 9 18 2013) (Date 10 18 2013)) #f)
(check-expect (date>? (Date 10 25 2012) (Date 10 25 2013)) #f)
(check-expect (date>? (Date 11 3 2013) (Date 11 18 2013)) #f)
(check-expect (date>? (Date 12 19 2013) (Date 12 19 2013)) #f)
(check-expect (date>? (Date 9 18 2013) (Date 10 18 2003)) #t)
(check-expect (date>? (Date 10 25 2013) (Date 10 2 2013)) #t)
(check-expect (date>? (Date 11 3 2013) (Date 3 18 2013)) #t)

;; Problem 2
;;
;; Representation of a cubic polynomial
;;
(define-struct Cubic
  ([a : Exact-Rational]     ;; a value
   [b : Exact-Rational]     ;; b value
   [c : Exact-Rational]     ;; c value
   [d : Exact-Rational]))   ;; d value

(: eval-cubic : Cubic Real -> Real)
;; evaluates a function f at x, to give f(x)
;;
(define (eval-cubic f x)
  (+ (* (Cubic-a f) (* x x x))
     (* (Cubic-b f) (* x x))
     (* (Cubic-c f) x)
     (Cubic-d f)))
(check-expect (eval-cubic (Cubic 2 3 4 1) 5) 346)
(check-expect (eval-cubic (Cubic 1 3 9 3) 5) 248)
(check-expect (eval-cubic (Cubic 3 -5 6 -25) 3) 29)
(check-expect (eval-cubic (Cubic -1 7 2 0) -6) 456)
(check-expect (eval-cubic (Cubic 2 8 4 1) 1) 15)

(: cubic-string : Cubic -> String)
;; turns a cubic function into a readable string
;;
(define (cubic-string f)
  (local
    {(define a : String (number->string (Cubic-a f)))
     (define b : String (number->string (Cubic-b f)))
     (define c : String (number->string (Cubic-c f)))
     (define d : String (number->string (Cubic-d f)))}
    (string-append a "*x^3 + " b "x^2 + " c "x + " d)))
(check-expect (cubic-string (Cubic 2 3 4 1)) "2*x^3 + 3x^2 + 4x + 1")
(check-expect (cubic-string (Cubic 6 2 1 -4)) "6*x^3 + 2x^2 + 1x + -4")
(check-expect (cubic-string (Cubic -1 4 9 0)) "-1*x^3 + 4x^2 + 9x + 0")

;; defines a quadratic type
;;
(define-struct Quadratic
  ([a : Exact-Rational]            ;; a value
   [b : Exact-Rational]            ;; b value
   [c : Exact-Rational]))          ;; c value

(: derivative : Cubic -> Quadratic)
;; computes the derivative of the cubic function
;;
(define (derivative f)
  (local
    {(define a : Exact-Rational (Cubic-a f))
     (define b : Exact-Rational (Cubic-b f))
     (define c : Exact-Rational (Cubic-c f))}
    (Quadratic (* a 3) (* b 2) c)))
(check-expect (derivative (Cubic 2 3 4 1)) (Quadratic 6 6 4))
(check-expect (derivative (Cubic 1 3 9 3)) (Quadratic 3 6 9))
(check-expect (derivative (Cubic 3 -5 6 -25)) (Quadratic 9 -10 6))
(check-expect (derivative (Cubic -1 7 2 0)) (Quadratic -3 14 2))
(check-expect (derivative (Cubic 2 8 4 1)) (Quadratic 6 16 4))

;; Problem 3
;;
(: trinomial : Integer Integer -> Integer)
;; returns the value of the trinomial triangle at the given location
;;
(define (trinomial n i)
  (cond
    [(< i 0) 0]
    [(and (= 0 n) (not (= i 0))) 0]
    [(or (= i 0) (= i (* n 2))) 1]
    [else (+ (trinomial (- n 1) (- i 2))
             (trinomial (- n 1) (- i 1))
             (trinomial (- n 1) i))]))
(check-expect (trinomial 3 6) 1)
(check-expect (trinomial 4 5) 16) 
(check-expect (trinomial 4 4) 19) 
(check-expect (trinomial 0 0) 1)
(check-expect (trinomial 0 1) 0)
(check-expect (trinomial 2 1) 2) 

(: string-row : Integer Integer -> String)
;; helper function to create a string of the given trinomial triangle row
;; attaches the i element to the accumulator string starting at the given position
;;
(define (string-row n i)
  (if (= i (* n 2))
      (number->string (trinomial n i))
      (string-append (number->string (trinomial n i)) " " (string-row n (+ i 1)))))
(check-expect (string-row 3 3) "7 6 3 1")
(check-expect (string-row 4 0) "1 4 10 16 19 16 10 4 1")
(check-expect (string-row 0 0) "1")

(: trinomial-row : Integer -> String)
;; returns the string representation of given Trinomial triangle row
;;
(define (trinomial-row row)
  (local
    {(define i : Integer 0)}
    (string-row row i))) 
(check-expect (trinomial-row 0) "1")
(check-expect (trinomial-row 4) "1 4 10 16 19 16 10 4 1")
(check-expect (trinomial-row 3) "1 3 6 7 6 3 1")
(check-expect (trinomial-row 2) "1 2 3 2 1")

;; run tests
;;
(test)