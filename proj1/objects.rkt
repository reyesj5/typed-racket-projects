#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 1 -- Objects module
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

;; load vector module
;;
(require "vec3.rkt")

;; load material module
;;
(require "material.rkt")

;; For now, we only have spheres
(define-type Object Sphere)

;; value represents a sphere
;;
(define-struct Sphere
  ([c : Vec3]               ;; position given by c
   [r : Real]               ;; radius r
   [r^2 : Real]             ;; radius squared r^2
   [mat : Material]))       ;; surface material mat

(: make-sphere : Vec3 Real Material -> Object)
;; takes a position, radius, and material are returns a sphere
;;
(define (make-sphere c r mat)
  (Sphere c r (* r r) mat))

;; sample spheres to test functions
;;
(define sample-sphere : Sphere (make-sphere (Vec3 0 0 0) 5 (Material (RGB 1 1 1))))
(define sample-sphere2 : Sphere (make-sphere (Vec3 2 1 3) 2 (Material (RGB 1 1 1))))

;; A (Hit t obj) represents an intersection between a ray R
;; and the object obj at point R(t).
(define-struct Hit
  ([t : Real]
   [obj : Object]))

;; A Maybe-Hit is either a Hit or 'miss
(define-type Maybe-Hit (U Hit 'miss))

(: ray-sphere-intersect : Ray -> Sphere -> Maybe-Hit)
;; tests the ray against the sphere for intersection and returns the closest
;; hit point if there is an intersection or returns 'miss if there is no intersection. 
(define (ray-sphere-intersect ray)
  (local
    {(: sphere-intersect : Sphere -> Maybe-Hit)
     ;; tests the ray against sphere for intersection
     ;;
     (define (sphere-intersect sphere)
       (local
         {(define q : Vec3 (vec3- (Ray-origin ray)(Sphere-c sphere)))
          (define q-dot : Real (vec3-dot q q))
          (define b : Real (* 2 (vec3-dot q (Ray-dir ray))))
          (define r^2 : Real (Sphere-r^2 sphere))
          (define D : Real (- (* b b) (* 4 1 (- q-dot r^2))))
          (define t : Real (/ (- (* -1 b) (sqrt (abs D))) (* 2 1)))}
         (if (or (<= q-dot r^2) (< D 0) (< t 0))
             'miss
             (Hit t sphere))))}
    sphere-intersect))

(check-expect ((ray-sphere-intersect (make-ray (Vec3 0 0 0) (Vec3 1 1 1))) sample-sphere) 'miss)
(check-within ((ray-sphere-intersect (make-ray (Vec3 8 6 7) (Vec3 -1 -1 -1))) sample-sphere)
              (Hit 7.3285 sample-sphere) 0.001)
(check-expect ((ray-sphere-intersect (make-ray (Vec3 1 2 1) (Vec3 1 2 3))) sample-sphere) 'miss)
(check-expect ((ray-sphere-intersect (make-ray (Vec3 3 3 3) (Vec3 0.1 0.1 0.1))) sample-sphere) 'miss)
(check-expect ((ray-sphere-intersect (make-ray (Vec3 3 3 3) (Vec3 1 1 1))) sample-sphere) 'miss)
(check-within ((ray-sphere-intersect (make-ray (Vec3 3 3 3) (Vec3 -5 -5 -5))) sample-sphere)
              (Hit 0.196152 sample-sphere) 0.00001)

(: sphere-normal : Sphere Vec3 -> Vec3)
;; returns the surface-normal vector for the given sphere and intersection point
;;
(define (sphere-normal sphere p)
  (local
    {(define p-c : Vec3 (vec3- p (Sphere-c sphere)))}
    (vec3-scale (/ 1 (Sphere-r sphere)) p-c)))

(check-expect (sphere-normal sample-sphere (Vec3 5 5 5)) (Vec3 1 1 1))
(check-expect (sphere-normal sample-sphere2 (Vec3 6 5 5)) (Vec3 2 2 1))
(check-within (sphere-normal sample-sphere2 (Vec3 8 2 7)) (Vec3 3 .5 2) .01)
(check-expect (sphere-normal sample-sphere (Vec3 4 2 3)) (Vec3 4/5 2/5 3/5))
(check-expect (sphere-normal sample-sphere2 (Vec3 6 2 5)) (Vec3 2 1/2 1))
(check-within (sphere-normal sample-sphere2 (Vec3 1 2 7)) (Vec3 -1/2 .5 2) .01)

(: sphere-material : Sphere -> Material)
;; returns the sphere's material
;;
(define (sphere-material sphere)
  (Sphere-mat sphere))
(check-expect (sphere-material sample-sphere) (Material (RGB 1 1 1)))
(check-expect (sphere-material sample-sphere2) (Material (RGB 1 1 1)))

(: ray-object-intersect : Ray -> Object -> Maybe-Hit)
;; test for a ray-object intersection
;;
(define ray-object-intersect ray-sphere-intersect)

(: object-normal : Object Vec3 -> Vec3)
;; return the unit surface normal for the given point on the object
;;
(define object-normal sphere-normal)
  
(: object-material : Object -> Material)
;; return the object's surface material
;;
(define object-material sphere-material)

(test)

;; Exports
;;;;;;;;;;

(provide Object
         (struct-out Hit)
         Maybe-Hit)

(provide make-sphere
         ray-object-intersect
         object-normal
         object-material)