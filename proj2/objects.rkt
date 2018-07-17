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
(define-type Object (U Sphere Plane))

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
(define sample-sphere : Sphere (Sphere (Vec3 0 0 0) 5 25 (make-diffuse (RGB 1 1 1))))
(define sample-sphere2 : Sphere (Sphere (Vec3 2 1 3) 2 4 (make-diffuse (RGB 1 1 1))))

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
(check-expect (sphere-material sample-sphere) (make-diffuse (RGB 1 1 1)))
(check-expect (sphere-material sample-sphere2) (make-diffuse (RGB 1 1 1)))

;; Project 2
;;

(define-struct Plane
  ([n : Vec3]
   [signed-dist : Real]
   [mat : Material]))

(: make-plane : Vec3 Real Material -> Object)
;; makes a plane from given arguments
;;
(define (make-plane v dist mat)
  (Plane (vec3-normalize v) dist mat))

(: ray-plane-intersect : Ray -> Plane -> Maybe-Hit)
;; tests a ray against a plane for intersection and returns the Hit
;; if there is an intersection or returns 'miss if there is no intersection
;;
(define (ray-plane-intersect ray)
  (local
    {(: plane-intersect : Plane -> Maybe-Hit)
     ;; tests a plane against a ray for a hit
     ;;
     (define (plane-intersect plane)
       (local
         {(define c : Real (vec3-dot (Ray-dir ray) (Plane-n plane)))
          (define t : Real (/ (- (Plane-signed-dist plane)
                              (vec3-dot (Plane-n plane) (Ray-origin ray))) c))}
         (if (< -0.00001 c)
             'miss
             (if (<= t 0) 'miss (Hit t plane)))))}
    plane-intersect))

(check-expect ((ray-plane-intersect (make-ray (Vec3 5 3 0) (Vec3 4 2 3)))
               (Plane (Vec3 4 1 2) 55 (make-diffuse (RGB 5 5 5))))
              'miss)

(: ray-object-intersect : Ray -> Object -> Maybe-Hit)
;; test for a ray-object intersection
;;
(define (ray-object-intersect ray)
  (local
    {(: check-object : Object -> Maybe-Hit)
     ;; checks which object we are dealing with and selects function
     ;;
     (define (check-object obj)
       (match obj
         [(Sphere _ _ _ _) ((ray-sphere-intersect ray) obj)]
         [(Plane _ _ _) ((ray-plane-intersect ray) obj)]))}
    check-object))

(: object-normal : Object Vec3 -> Vec3)
;; return the unit surface normal for the given point on the object
;;
(define (object-normal obj v)
  (match obj
    [(Sphere _ _ _ _) (sphere-normal obj v)]
    [(Plane n _ _) n]))
  
(: object-material : Object -> Material)
;; return the object's surface material
;;
(define (object-material obj)
  (match obj
    [(Sphere _ _ _ _) (sphere-material obj)]
    [(Plane _ _ _) (Plane-mat obj)]))

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

;; Project 2 Exports
;;;;;;;;;;;;;;;;;;;;

(provide make-plane)