#lang typed/racket

;; CMSC15100 Winter 2017
;; Homework 4
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; load image library definitions
;;
(require "../include/uc151image.rkt")

;; Problem 1
;;

;; represents vectors in 3D-space
;;
(define-struct Vec3
  ([x : Real]    ;; x component
   [y : Real]    ;; y component
   [z : Real]))  ;; z component

;; the zero vector
;;
(define vec3-zero : Vec3 (Vec3 0 0 0))

(: vec3-negate : Vec3 -> Vec3)
;; negates all components of a vector
;;
(define (vec3-negate vector)
  (Vec3 (* -1 (Vec3-x vector)) (* -1 (Vec3-y vector)) (* -1 (Vec3-z vector))))
(check-expect (vec3-negate (Vec3 2 5 -6)) (Vec3 -2 -5 6))
(check-expect (vec3-negate (Vec3 -4 -2 5)) (Vec3 4 2 -5))
(check-expect (vec3-negate (Vec3 -1 -9 0)) (Vec3 1 9 0))

(: vec3+ : Vec3 Vec3 -> Vec3)
;; adds two vectors, yielding a new vector
;;
(define (vec3+ v1 v2)
  (Vec3 (+ (Vec3-x v1) (Vec3-x v2))
        (+ (Vec3-y v1) (Vec3-y v2))
        (+ (Vec3-z v1) (Vec3-z v2))))
(check-expect (vec3+ (Vec3 2 5 -6) (Vec3 22 1 0)) (Vec3 24 6 -6))
(check-expect (vec3+ (Vec3 -4 -2 5) (Vec3 7 5 9)) (Vec3 3 3 14))
(check-expect (vec3+ (Vec3 -1 -9 0) (Vec3 -2 6 1)) (Vec3 -3 -3 1))

(: vec3- : Vec3 Vec3 -> Vec3)
;; subtracts two vectors, yielding a new vector
;;
(define (vec3- v1 v2)
  (Vec3 (- (Vec3-x v1) (Vec3-x v2))
        (- (Vec3-y v1) (Vec3-y v2))
        (- (Vec3-z v1) (Vec3-z v2))))
(check-expect (vec3- (Vec3 2 5 -6) (Vec3 22 1 0)) (Vec3 -20 4 -6))
(check-expect (vec3- (Vec3 -4 -2 5) (Vec3 7 5 9)) (Vec3 -11 -7 -4))
(check-expect (vec3- (Vec3 -1 -9 0) (Vec3 -2 6 1)) (Vec3 1 -15 -1))

(: vec3-scale : Real Vec3 -> Vec3)
;; takes a scalar s (Real) and a vector v and returns a new vector
;; with all of the components of v multiplied by s.
;;
(define (vec3-scale s vector)
  (Vec3 (* s (Vec3-x vector)) (* s (Vec3-y vector)) (* s (Vec3-z vector))))
(check-expect (vec3-scale -2 (Vec3 22 1 0)) (Vec3 -44 -2 0))
(check-expect (vec3-scale 5 (Vec3 7 5 9)) (Vec3 35 25 45))
(check-expect (vec3-scale 0 (Vec3 -2 6 1)) (Vec3 0 0 0))

(: vec3-dot : Vec3 Vec3 -> Real)
;; computes the dot product of two vectors
;;
(define (vec3-dot v1 v2)
  (+ (* (Vec3-x v1) (Vec3-x v2))
     (* (Vec3-y v1) (Vec3-y v2))
     (* (Vec3-z v1) (Vec3-z v2))))
(check-expect (vec3-dot (Vec3 2 5 -6) (Vec3 22 1 0)) 49)
(check-expect (vec3-dot (Vec3 -4 -2 5) (Vec3 7 5 9)) 7)
(check-expect (vec3-dot (Vec3 -1 -9 0) (Vec3 -2 6 1)) -52)

(: vec3-length : Vec3 -> Real)
;; computes the length (magnitude) of a vector
;;
(define (vec3-length v1)
  (sqrt (vec3-dot v1 v1)))
(check-within (vec3-length (Vec3 2 5 -6)) 8.06225 .0001)
(check-within (vec3-length (Vec3 -4 -2 5)) 6.70820 .0001)
(check-within (vec3-length (Vec3 -1 -9 0)) 9.05538 .0001)

(: vec3-normalize : Vec3 -> Vec3)
;; normalizes the vector
;;
(define (vec3-normalize v1)
  (local
    {(define length : Real (vec3-length v1))}
    (if (< length .0001)
      vec3-zero
      (Vec3 (/ (Vec3-x v1) length)
            (/ (Vec3-y v1) length)
            (/ (Vec3-z v1) length)))))
(check-expect (vec3-normalize (Vec3 .00001 .00003 .00006)) (Vec3 0 0 0))
(check-within (vec3-normalize (Vec3 2 5 -6)) (Vec3 .248069 .620174 -.744209) .0001)
(check-within (vec3-normalize (Vec3 -4 -2 5)) (Vec3 -.596285 -.298142 .74535640) .0001)

;; Problem 2
;;

(: thirds : (Listof Exact-Rational) -> (Listof Exact-Rational))
;; divide all numbers in the list by three
;;
(define (thirds list)
  (match list
    ['() '()]
    [(cons x rest) (cons (/ x 3) (thirds rest))]))
(check-expect (thirds (list 9 27 81)) (list 3 9 27))
(check-expect (thirds (list -9 21 63)) (list -3 7 21))
(check-expect (thirds (list 15 18 90)) (list 5 6 30))

(: add-to-list : Number (Listof Number) -> (Listof Number))
;; adds the first argument to each element of the second argument
;;
(define (add-to-list n list)
  (match list
    ['() '()]
    [(cons x rest) (cons (+ x n) (add-to-list n rest))]))
(check-expect (add-to-list 25 (list 9 27 81)) (list 34 52 106))
(check-expect (add-to-list 10 (list -9 21 63)) (list 1 31 73))
(check-expect (add-to-list 8 (list 15 18 90)) (list 23 26 98))

(: non-zeros : (Listof Integer) -> (Listof Integer))
;; filters out any occurrences of 0 in the list
;;
(define (non-zeros list)
  (match list
    ['() '()]
    [(cons x rest) (if (= x 0) (non-zeros rest) (cons x (non-zeros rest)))]))
(check-expect (non-zeros (list 9 0 27 0 0 81 1)) (list 9 27 81 1))
(check-expect (non-zeros (list -9 0 21 1 0 63 0)) (list -9 21 1 63))
(check-expect (non-zeros (list 0 15  2 0 18 90)) (list 15 2 18 90))

(: larger-than : Real (Listof Real) -> (Listof Real))
;; keeps numbers above threshold, discards others
;;
(define (larger-than n list)
  (match list
    ['() '()]
    [(cons x rest) (if (<= x n) (larger-than n rest) (cons x (larger-than n rest)))]))
(check-expect (larger-than 0 (list 9 0 27 0 0 81 1)) (list 9 27 81 1))
(check-expect (larger-than 5 (list -9 0 21 1 0 63 0)) (list 21 63))
(check-expect (larger-than 20 (list 0 15  2 0 18 90)) (list 90))

(: replicate : (All (A) (Integer A -> (Listof A))))
;; takes a length n and a value v, and returns a list
;; of length n where every element in the list is v.
;;
(define (replicate n a)
  (if (<= n 0) '() (cons a (replicate (- n 1) a))))
(check-expect (replicate 4 "hello") (list "hello" "hello" "hello" "hello"))
(check-expect (replicate 5 2) (list 2 2 2 2 2))
(check-expect (replicate 2 'red) (list 'red 'red))

(: wider-than : Real (Listof Image) -> (Listof Image))
;; keep the images that are wider than the given threshold, discards the others.
;;
(define (wider-than n list)
  (match list
    ['() '()]
    [(cons image rest) (if (<= (image-width image) n)
                           (wider-than n rest)
                           (cons image (wider-than n rest)))]))
(check-expect (wider-than 50 (list (square 50 "solid" "green")
                                   (square 80 'outline "red")
                                   (circle 20 'solid "blue")
                                   (triangle 90 'solid "black")))
              (list (square 80 "outline" "red") (triangle 90 "solid" "black")))
(check-expect (wider-than 80 (list (square 50 "solid" "green")
                                   (square 80 'outline "red")
                                   (circle 20 'solid "blue")
                                   (triangle 90 'solid "black")))
              (list (triangle 90 "solid" "black")))
(check-expect (wider-than 100 (list (square 50 "solid" "green")
                                   (square 80 'outline "red")
                                   (circle 20 'solid "blue")
                                   (triangle 90 'solid "black")))
              '())

(: list-sum : (Listof Number) -> Number)
;; compute the sum of the numbers in the list
;;
(define (list-sum list)
  (local
    {(: accumulator : Number (Listof Number) -> Number)
     ;; recursively adds each value in the list
     ;;
     (define (accumulator n aList)
       (match aList
         ['() n]
         [(cons x rest) (accumulator (+ n x) rest)]))}
    (accumulator 0 list)))
(check-expect (list-sum (list 9 2 27 1 81 5)) 125)
(check-expect (list-sum (list -9 21 63 9 5 0)) 89)
(check-expect (list-sum (list 15 18 90 8 7 3)) 141)

(: list-xor : (Listof Boolean) -> Boolean)
;; returns true if exactly one item in the list is true and false otherwise
;;
(define (list-xor list)
  (local
    {(: true-count : Integer (Listof Boolean) -> Integer)
     ;; counts the number of true values in the list
     ;;
     (define (true-count n list)
       (match list
         ['() n]
         [(cons #t rest) (true-count (+ n 1) rest)]
         [(cons _ rest) (true-count n rest)]))
     (define trues : Integer (true-count 0 list))}
    (= 1 trues)))
(check-expect (list-xor '()) #f)
(check-expect (list-xor (list #f #f #f #t)) #t)
(check-expect (list-xor (list true false true false #t #f)) #f)

(: that-many : (Listof Integer) -> (Listof (Listof Integer)))
;; builds a list of lists containing "that many" of each
;;
(define (that-many list)
  (match list
    ['() '()]
    [(cons x rest) (if (< x 1)
                       (cons '() (that-many rest))
                       (cons (replicate x x) (that-many rest)))]))
(check-expect (that-many '(-2 1 3)) '(() (1) (3 3 3)))
(check-expect (that-many '(2 1 3)) '((2 2) (1) (3 3 3)))
(check-expect (that-many '(3 1 5 0)) '((3 3 3) (1) (5 5 5 5 5) ()))

(: take : (All (A) (Listof A) Integer -> (Listof A)))
;; given a list xs and an integer n, this function returns the first n elements of xs.
;; if n is less than or equal to zero, then it returns the empty list.
(define (take list n)
  (local
    {(: obtain-values : Integer (Listof A) -> (Listof A))
     ;; uses recursion to get the n elements from the list
     ;;
     (define (obtain-values n list)
       (if (< n 1)
           '()
           (match list
             ['() '()]
             [(cons x rest) (cons x (obtain-values (- n 1) rest))])))}
    (obtain-values n list)))
(check-expect (take '(1 2 3 4 5 6 7 8 9 10) 5) '(1 2 3 4 5))
(check-expect (take '(#t #f #f #t #f #t #f) 6) '(#t #f #f #t #f #t))
(check-expect (take (list "hello" "world" "I" "am" "done") 4) (list "hello" "world" "I" "am"))

;; run tests
;;
(test)