#lang typed/racket

;; CMSC15100 Winter 2017
;; Homework 3
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; Problem 1
;;

;; A 2D point
(define-struct Point
  ([x : Real]
   [y : Real]))

;; A Dataset is either an 'EmptyDataset or a (Sample obs ds), where
;; obs is an observation (represented as a Point) and ds is a
;; Dataset.
(define-type Dataset
  (U 'EmptyDataset Sample))
(define-struct Sample
  ([obs : Point]
   [dat : Dataset]))

(: dataset-size : Dataset -> Integer)
;; returns the size of the data set
;;
(define(dataset-size set)
  (if(Sample? set)
     (+ 1 (dataset-size (Sample-dat set)))
     0))
(check-expect (dataset-size (Sample (Point 0 0) 'EmptyDataset)) 1)
(check-expect (dataset-size 'EmptyDataset) 0)
(check-expect (dataset-size (Sample (Point 0 0) (Sample (Point 2 5)'EmptyDataset))) 2)

(: dataset-min : Dataset -> Point)
;; returns the Point with the min-x and min-y values in the data set.
;;
(define (dataset-min data)
  (local
    {(: find-min : Dataset Real Real -> Point)
     ;; uses accumulators and recursion to keep track of min values for x and y
     ;;
     (define (find-min rest x y)
       (cond
         [(equal? 'EmptyDataset rest) (Point x y)]
         [(and (< (Point-x (Sample-obs rest)) x)
               (< (Point-y (Sample-obs rest)) y))
          (find-min (Sample-dat rest)
                    (Point-x (Sample-obs rest))
                    (Point-y (Sample-obs rest)))]
         [(< (Point-x (Sample-obs rest)) x)
          (find-min (Sample-dat rest) (Point-x (Sample-obs rest)) y)]
         [(< (Point-y (Sample-obs rest)) y)
          (find-min (Sample-dat rest) x (Point-y (Sample-obs rest)))]
         [else (find-min (Sample-dat rest) x y)]))}
    (match data
      ['EmptyDataset (error "dataset-min: empty dataset")]
      [(Sample p rest) (find-min rest (Point-x p) (Point-y p))])))
(check-expect (dataset-min (Sample (Point 5 1) (Sample (Point 2 10) 'EmptyDataset))) (Point 2 1))
(check-expect (dataset-min (Sample (Point 5 11) (Sample (Point -5 10) 'EmptyDataset))) (Point -5 10))
(check-expect (dataset-min (Sample (Point 5 155)
                                (Sample (Point 25 10)
                                        (Sample (Point 25 500) 'EmptyDataset)))) (Point 5 10))
(check-error (dataset-min 'EmptyDataset) "dataset-min: empty dataset")

(: dataset-adjust : Dataset -> Dataset)
;; computes the minimum x and y values and then adjusts the values in the dataset
;;
(define (dataset-adjust set)
  (local
    {(define min-x : Real (if (Sample? set) (Point-x (dataset-min set)) 0))
     (define min-y : Real (if (Sample? set) (Point-y (dataset-min set)) 0))
     (: adjust : Dataset Real Real -> Dataset)
     ;; recursively adjusts every value in the data set by given min x and y values
     ;;
     (define (adjust data x y)
       (match data
         ['EmptyDataset 'EmptyDataset]
         [(Sample p rest) (Sample (Point (- (Point-x p) x) (- (Point-y p) y)) (adjust rest x y))]))}
    (adjust set min-x min-y)))
(check-expect (dataset-adjust
               (Sample (Point 1 2) (Sample (Point 3 -1) 'EmptyDataset)))
              (Sample (Point 0 3) (Sample (Point 2 0) 'EmptyDataset)))
(check-expect (dataset-adjust 'EmptyDataset) 'EmptyDataset)
(check-expect (dataset-adjust
               (Sample (Point 5 2) (Sample (Point 3 6) 'EmptyDataset)))
              (Sample (Point 2 0) (Sample (Point 0 4) 'EmptyDataset)))

;; Problem 2
;;

;; A line has a slope and y-axis intercept
(define-struct Line
  ([slope : Real]
   [intercept : Real]))



(: compute-line : Integer Real Real Real Real -> Line)
;; compute the linear-regression line.  The argumente are the number of samples,
;; the sum of x_i, the sum of y_i, the sum of x_i*y_i, and the sum of the
;; squares of x_i.  If the number of elements is less than 2, this function
;; signals an error.
(define (compute-line n xi yi xy xx)
  (local
    {(define slope : Real (/ (- (* n xy) (* xi yi)) (- (* n xx) (* xi xi))))}
  (if (< n 2)
      (error "compute-line: neet at least two values")
      (Line slope (- (/ yi n) (* slope (/ xi n)))))))
(check-within (compute-line 11 99 82.51 797.6 1001) (Line .5 3) .01)
(check-within (compute-line 11 99 82.51 797.59 1001) (Line .5 3) .01)
(check-within (compute-line 11 99 82.5 797.47 1001) (Line .5 3) .01)
(check-within (compute-line 11 99 82.51 797.58 1001) (Line .5 3) .01)

(: compute-sums : Dataset Integer Real Real Real Real -> Line)
;; computes the sums of x_i, the sum of y_i, the sum of x_i*y_i, and the sum of the
;; squares of x_i.  If the number of elements is less than 2, this function
;; signals an error.
(define (compute-sums data n xi yi xy xx)
  (if (Sample? data)
      (compute-sums (Sample-dat data)
                    (+ n 1)
                    (+ (Point-x (Sample-obs data)) xi)
                    (+ (Point-y (Sample-obs data)) yi)
                    (+ (* (Point-x (Sample-obs data)) (Point-y (Sample-obs data))) xy)
                    (+ (* (Point-x (Sample-obs data)) (Point-x (Sample-obs data))) xx))
      (compute-line n xi yi xy xx)))
(check-expect
 (compute-sums
  (Sample (Point 1 1) (Sample (Point 2 2) 'EmptyDataset))
  0
  0
  0
  0
  0)
 (Line 1 0))
(check-within (compute-sums
               (Sample (Point 8 6.58)
                (Sample (Point 8 5.76)
                 (Sample (Point 8 7.71)
                  (Sample (Point 8 8.84)
                   (Sample (Point 8 8.47)
                    (Sample (Point 8 7.04)
                     (Sample (Point 8 5.25)
                      (Sample (Point 8 7.91)
                       (Sample (Point 19 12.50)
                        (Sample (Point 8 5.56)
                         (Sample (Point 8 6.89)
                          'EmptyDataset))))))))))) 0 0 0 0 0)
              (Line 0.5 3) 0.01)
(check-within (compute-sums
               (Sample (Point 10 8.04)
                (Sample (Point 8 6.95)
                 (Sample (Point 13 7.58)
                  (Sample (Point 9 8.81)
                   (Sample (Point 11 8.33)
                    (Sample (Point 14 9.96)
                     (Sample (Point 6 7.24)
                      (Sample (Point 4 4.26)
                       (Sample (Point 12 10.84)
                        (Sample (Point 7 4.82)
                         (Sample (Point 5 5.68)
                          'EmptyDataset))))))))))) 0 0 0 0 0)
              (Line 0.5 3) 0.01)
(check-within (compute-sums
               (Sample (Point 10 9.14)
                (Sample (Point 8 8.14)
                 (Sample (Point 13 8.74)
                  (Sample (Point 9 8.77)
                   (Sample (Point 11 9.26)
                    (Sample (Point 14 8.1)
                     (Sample (Point 6 6.13)
                      (Sample (Point 4 3.1)
                       (Sample (Point 12 9.13)
                        (Sample (Point 7 7.26)
                         (Sample (Point 5 4.74)
                          'EmptyDataset))))))))))) 0 0 0 0 0)
              (Line 0.5 3) 0.01)
(check-within (compute-sums
               (Sample (Point 10 7.46)
                (Sample (Point 8 6.77)
                 (Sample (Point 13 12.74)
                  (Sample (Point 9 7.11)
                   (Sample (Point 11 7.81)
                    (Sample (Point 14 8.84)
                     (Sample (Point 6 6.08)
                      (Sample (Point 4 5.39)
                       (Sample (Point 12 8.15)
                        (Sample (Point 7 6.42)
                         (Sample (Point 5 5.73)
                          'EmptyDataset))))))))))) 0 0 0 0 0)
              (Line 0.5 3) 0.01)

(: linear-regression : Dataset -> Line)
;; computes the best-fit line for the dataset
;;
(define (linear-regression data)
  (compute-sums data 0 0 0 0 0))
(check-within (linear-regression
               (Sample (Point 8 6.58)
                (Sample (Point 8 5.76)
                 (Sample (Point 8 7.71)
                  (Sample (Point 8 8.84)
                   (Sample (Point 8 8.47)
                    (Sample (Point 8 7.04)
                     (Sample (Point 8 5.25)
                      (Sample (Point 8 7.91)
                       (Sample (Point 19 12.50)
                        (Sample (Point 8 5.56)
                         (Sample (Point 8 6.89)
                          'EmptyDataset))))))))))))
              (Line 0.5 3) 0.01)
(check-within (linear-regression
               (Sample (Point 10 8.04)
                (Sample (Point 8 6.95)
                 (Sample (Point 13 7.58)
                  (Sample (Point 9 8.81)
                   (Sample (Point 11 8.33)
                    (Sample (Point 14 9.96)
                     (Sample (Point 6 7.24)
                      (Sample (Point 4 4.26)
                       (Sample (Point 12 10.84)
                        (Sample (Point 7 4.82)
                         (Sample (Point 5 5.68)
                          'EmptyDataset))))))))))))
              (Line 0.5 3) 0.01)
(check-within (linear-regression
               (Sample (Point 10 9.14)
                (Sample (Point 8 8.14)
                 (Sample (Point 13 8.74)
                  (Sample (Point 9 8.77)
                   (Sample (Point 11 9.26)
                    (Sample (Point 14 8.1)
                     (Sample (Point 6 6.13)
                      (Sample (Point 4 3.1)
                       (Sample (Point 12 9.13)
                        (Sample (Point 7 7.26)
                         (Sample (Point 5 4.74)
                          'EmptyDataset))))))))))))
              (Line 0.5 3) 0.01)
(check-within (linear-regression
               (Sample (Point 10 7.46)
                (Sample (Point 8 6.77)
                 (Sample (Point 13 12.74)
                  (Sample (Point 9 7.11)
                   (Sample (Point 11 7.81)
                    (Sample (Point 14 8.84)
                     (Sample (Point 6 6.08)
                      (Sample (Point 4 5.39)
                       (Sample (Point 12 8.15)
                        (Sample (Point 7 6.42)
                         (Sample (Point 5 5.73)
                          'EmptyDataset))))))))))))
              (Line 0.5 3) 0.01)

;; run tests
;;
(test)