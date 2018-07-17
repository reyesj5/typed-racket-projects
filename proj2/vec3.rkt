#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 1 -- Vector module
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; Vectors
;;;;;;;;;;

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
      (vec3-scale (/ 1 length) v1))))

(check-expect (vec3-normalize (Vec3 .00001 .00003 .00006)) (Vec3 0 0 0))
(check-within (vec3-normalize (Vec3 2 5 -6)) (Vec3 .248069 .620174 -.744209) .0001)
(check-within (vec3-normalize (Vec3 -4 -2 5)) (Vec3 -.596285 -.298142 .74535640) .0001)

;; Rays
;;;;;;;;;;

;; A (Ray origin dir) represents a ray in 3D space, where origin
;; is the origin of the ray and dir is a unit vector specifying
;; the direction.
(define-struct Ray
  ([origin : Vec3]  ;; the origin of the ray
   [dir : Vec3]))   ;; the direction of the ray (unit vector)

(: make-ray : Vec3 Vec3 -> Ray)
;; make a ray with the given origin and direction vector.
;; This function normalizes the direction to a unit vector
(define (make-ray pt dir)
  (Ray pt (vec3-normalize dir)))

(: ray-point-at : Ray Real -> Vec3)
;; return the point on the ray at the given distance
;; from the origin
(define (ray-point-at ray t)
  (match ray
    [(Ray origin dir) (vec3+ origin (vec3-scale t dir))]))

(: ray-offset : Ray Real -> Ray)
;; offset the origin of the ray by the given distance
(define (ray-offset ray t)
  (Ray (ray-point-at ray t) (Ray-dir ray)))

;; Project 2
;;

(: vec3-reflect : Vec3 Vec3 -> Vec3)
;; given a vector v and a unit surface normal n, return
;; the reflection of v off of the surface.
;;
(define (vec3-reflect v v2)
  (local
    {(define n : Vec3 (vec3-normalize v2))}
  (vec3- v (vec3-scale (* 2 (vec3-dot v n)) n))))

(check-within (vec3-reflect (Vec3 2 2 2) (Vec3 1 1 0)) (Vec3 -2 -2 2) .00001)
(check-within (vec3-reflect (Vec3 5 5 5) (Vec3 1 1 1)) (Vec3 -5 -5 -5) .00001)
(check-within (vec3-reflect (Vec3 5 1 0) (Vec3 1 1 0)) (Vec3 -1 -5 0) .00001)
(check-within (vec3-reflect (Vec3 1 -1 0) (Vec3 0 1 0)) (Vec3 1 1 0) .00001)


(: vec3-halfway : Vec3 Vec3 -> Vec3)
;;
;;
(define (vec3-halfway v1 v2)
  (vec3-normalize (vec3+ v1 v2)))
(check-expect (vec3-halfway (Vec3 5 2 0) (Vec3 5 -2 0)) (Vec3 1 0 0))
(check-within (vec3-halfway (Vec3 2 2 2) (Vec3 5 5 5))
              (Vec3 0.57735 0.57735 0.57735) .0001)
(check-expect (vec3-halfway (Vec3 2 2 2) (Vec3 -2 -2 -2)) (Vec3 0 0 0))

(test)

;; Exports
;;;;;;;;;;

(provide (struct-out Vec3)
         (struct-out Ray))

(provide
 vec3-zero
 vec3-negate
 vec3+
 vec3-
 vec3-scale
 vec3-dot
 vec3-length
 vec3-normalize
 make-ray
 ray-point-at
 ray-offset)

;; Project 2 Exports
;;;;;;;;;;;;;;;;;;;;

(provide vec3-reflect
         vec3-halfway)