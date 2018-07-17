#lang typed/racket

;; CMSC15100 Winter 2017
;; Lab 2
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions (including testing support)
;;
(require "../include/uchicago151.rkt")

(: leap-year? : Integer -> Boolean)
;; Checks if argument is a leap-year
;;
(define (leap-year? y)
  (cond
    [(or (and (= (remainder y 4) 0) (not (= (remainder y 100) 0)))
         (= (remainder y 400) 0)) #t]
    [else #f]))
(check-expect (leap-year? 1904) #t)
(check-expect (leap-year? 1903) #f)
(check-expect (leap-year? 2020) #t)

(: valid-date? : Integer Integer Integer -> Boolean)
;; Determines if the arguments are a valid date
;;
(define (valid-date? m d y)
  (cond
    [(and (<= 1 m 12) (<= 1900 y 2099)
          (cond
            [(<= 1 d 28) #t]
            [(and  (or (= m 4) (= m 6) (= m 9) (= m 11)) (<= 1 m 30)) #t]
            [(and (not (leap-year? y)) (= m 2) (not (<= 1 d 28))) #f] 
            [(<= 1 d 31) #t]
            [else #f])) #t]
    [else #f]))
(check-expect (valid-date? 11 31 1997) #t)
(check-expect (valid-date? 2 29 2004) #t)
(check-expect (valid-date? 5 31 1905) #t)
(check-expect (valid-date? -1 8 2000) #f)
(check-expect (valid-date? 2 29 2001) #f)
(check-expect (valid-date? 5 0 2000) #f)

(: get-adj : Integer Integer -> Integer)
;; returns the adj value for the month argument of the given date
;;
(define (get-adj m y)
  (cond
    [(or (and (leap-year? y) (= m 1)) (= m 4) (= m 7)) 0]
    [(or (= m 1) (= m 10)) 1]
    [(= m 5) 2]
    [(or (and (leap-year? y) (= m 2)) (= m 8)) 3]
    [(or (= m 3) (= m 11)) 4]
    [(= m 6) 5]
    [else 6]))
(check-expect (get-adj 1 2000) 0)
(check-expect (get-adj 9 2003) 6)
(check-expect (get-adj 8 2003) 3)

(: day-of-week : Integer Integer Integer -> String)
;; returns the string name of the week day fom given date arguments
;;
(define (day-of-week m d y)
  (: n : Integer)
  ;; sets n equal to the calculation for the given equation
  ;;
  (define n (+ (- y 1900) (get-adj m y) d (exact-floor (/ y 4))))
  (: w : Integer)
  ;; sets w equal to the remainder of n divided by 7
  (define w (remainder n 7))
  (cond
   [(= w 1) "Monday"]
   [(= w 2) "Tuesday"]
   [(= w 3) "Wednesday"]
   [(= w 4) "Thursday"]
   [(= w 5) "Friday"]
   [(= w 6) "Saturday"]
   [else "Sunday"]))
(check-expect (day-of-week 1 11 2017) "Wednesday")
(check-expect (day-of-week 2 29 1996) "Thursday")
(check-expect (day-of-week 3 15 2013) "Friday")
(check-expect (day-of-week 4 27 2013) "Saturday")
(check-expect (day-of-week 5 5 2013) "Sunday")
(check-expect (day-of-week 6 10 2013) "Monday")
(check-expect (day-of-week 7 23 2013) "Tuesday")
(check-expect (day-of-week 8 22 2013) "Thursday")
(check-expect (day-of-week 9 18 2013) "Wednesday")
(check-expect (day-of-week 10 25 2013) "Friday")
(check-expect (day-of-week 11 3 2013) "Sunday")
(check-expect (day-of-week 12 19 2013) "Thursday")

(: date->string : Integer Integer Integer -> String)
;; converts given date into convenient string
;;
(define (date->string m d y)
  (: day : String)
  ;; sets day as the string of the day of the week
  ;;
  (define day (day-of-week m d y))
  (cond
    [(not (valid-date? m d y)) "[invalid date]"]
    [(= m 1) (string-append day " January "
               (number->string d) ", " (number->string y))]
    [(= m 2) (string-append day " February "
               (number->string d) ", " (number->string y))]
    [(= m 3) (string-append day " March "
               (number->string d) ", " (number->string y))]
    [(= m 4) (string-append day " April "
               (number->string d) ", " (number->string y))]
    [(= m 5) (string-append day " May "
               (number->string d) ", " (number->string y))]
    [(= m 6) (string-append day " June "
               (number->string d) ", " (number->string y))]
    [(= m 7) (string-append day " July "
               (number->string d) ", " (number->string y))]
    [(= m 8) (string-append day " August "
               (number->string d) ", " (number->string y))]
    [(= m 9) (string-append day " September "
               (number->string d) ", " (number->string y))]
    [(= m 10) (string-append day " October "
               (number->string d) ", " (number->string y))]
    [(= m 11) (string-append day " November "
               (number->string d) ", " (number->string y))]
    [else (string-append day " December "
               (number->string d) ", " (number->string y))]))
(check-expect (date->string 11 5 1997) "Wednesday November 5, 1997")
(check-expect (date->string 5 26 2017) "Friday May 26, 2017")
(check-expect (date->string 9 18 2017) "Monday September 18, 2017")
(check-expect (date->string 12 17 2017) "Sunday December 17, 2017")
(check-expect (date->string 6 20 2017) "Tuesday June 20, 2017")
(check-expect (date->string 8 19 2017) "Saturday August 19, 2017")
(check-expect (date->string 8 69 2017) "[invalid date]")

;; run tests
;;
(test)
