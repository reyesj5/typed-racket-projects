#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 1 -- Scene module
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

;; load objects module
;;
(require "objects.rkt")

;; a directional light source
;;
(define-struct Dir-Light
  ([dir : Vec3]         ;; unit vector pointing toward the light
   [intensity : RGB]))  ;; the light's intensity

;; generic type of lights
;;
(define-type Light Dir-Light)

(: make-dir-light : Vec3 RGB -> Light)
;; given a vector pointing toward the light and an intensity,
;; make a directional light source
(define (make-dir-light dir rgb)
  (Dir-Light (vec3-normalize dir) rgb))

(define-struct Scene
  ([background : RGB]
   [objects : (Listof Object)]
   [ambient : RGB]
   [lights : (Listof Light)]))

(: make-scene : RGB (Listof Object) RGB (Listof Light) -> Scene)
;; create a Scene; the arguments are background color, the list of
;; objects in the scene, the ambient light, and the list of light
;; sources in the scene.
(define make-scene Scene)

;; sample sphere to test functions
;;
(define sample-sphere : Object (make-sphere (Vec3 2 1 3) 2 (Material (RGB 1 1 1))))

;; sample scenes to test functions
;;
(define sample-scene : Scene
  (make-scene (RGB 50 50 50)
              (list (make-sphere (Vec3 0 0 0) 2 (Material (RGB 1 1 1)))
                    (make-sphere (Vec3 5 20 0) 1 (Material (RGB 1 1 1)))
                    (make-sphere (Vec3 0 10 6) 5 (Material (RGB 1 1 1))))
              (RGB 7 11 13)
              (list (make-dir-light (Vec3 2 1 5) (RGB 25 30 41)))))
(define sample-scene2 : Scene
  (make-scene (RGB 1 1 1)
              (list (make-sphere (Vec3 10 10 10) 20 (Material (RGB 1 1 1))))
              (RGB 10 50 90)
              (list (make-dir-light (Vec3 1 1 1) (RGB 1 1 1)))))
(define sample-scene3 : Scene
  (make-scene (RGB 169/255 169/255 169/255)
              (list (make-sphere (Vec3 1 -1 9) 2 (Material (RGB 1 1 16/17)))
                    (make-sphere (Vec3 -1/3 1 6) 3/4 (Material (RGB 1 1 16/17))))
              (RGB 0.1 0.1 0.0)
              (list (make-dir-light (Vec3 -1/2 1 -1) (RGB 1 0 0))
                    (make-dir-light (Vec3 1/2 1 -1) (RGB 0 1 0))
                    (make-dir-light (Vec3 5 2 6) (RGB 50 50 50)))))

(: nearest-hit : Maybe-Hit Maybe-Hit -> Maybe-Hit)
;; compares two Maybe-Hits and returns the nearest one
;;
(define (nearest-hit one two)
  (match one
    ['miss (if (Hit? two) two 'miss)]
    [Hit (if (Hit? two)
             (if (and (Hit? one) (<= (Hit-t one) (Hit-t two))) one two)
             one)]))

(check-expect (nearest-hit 'miss 'miss) 'miss)
(check-expect (nearest-hit 'miss (Hit 5 sample-sphere)) (Hit 5 sample-sphere))
(check-expect (nearest-hit (Hit 2 sample-sphere) 'miss) (Hit 2 sample-sphere))
(check-expect (nearest-hit (Hit 2 sample-sphere) (Hit 5 sample-sphere)) (Hit 2 sample-sphere))

(: closest-hit : Scene Ray -> Maybe-Hit)
;; takes a Scene and the initial ray that defines a pixel and traces the ray through
;; the scene and computes the color of the light that flows backwards along the ray’s path
;;
(define (closest-hit scene ray)
  (local
    {(define object-list : (Listof Object) (Scene-objects scene))
     (: helper : (Listof Object) Ray Maybe-Hit -> Maybe-Hit)
     ;; keeps an accumulator as a Maybe-Hit from the list of objects vs ray
     ;;
     (define (helper objects ray nearest)
       (match objects
         ['() nearest]
         [(cons x rest) (helper rest
                                ray
                                (nearest-hit nearest ((ray-object-intersect ray) x)))]))}
    (helper object-list ray 'miss)))

(check-within (closest-hit sample-scene (make-ray (Vec3 3 3 5) (Vec3 0 1 0)))
              (Hit 3.12701 (make-sphere (Vec3 0 10 6) 5 (Material (RGB 1 1 1)))) 0.0001)
(check-within (closest-hit sample-scene (make-ray (Vec3 1 14 0) (Vec3 0 -1 0)))
              (Hit 12.26794 (make-sphere (Vec3 0 0 0) 2 (Material (RGB 1 1 1)))) 0.0001)
(check-expect (closest-hit sample-scene (make-ray (Vec3 2 5 1) (Vec3 2 1 4))) 'miss)
(check-expect (closest-hit sample-scene2 (make-ray (Vec3 0 0 0) (Vec3 1 1 1))) 'miss)
(check-expect (closest-hit sample-scene2 (make-ray (Vec3 0 0 0) (Vec3 4 5 6))) 'miss)

(: any-hit? : Scene Ray -> Boolean)
;; returns #t on the first Hit and #f if there are no hits
;;
(define (any-hit? scene ray)
  (local
    {(define object-list : (Listof Object) (Scene-objects scene))
     (: find-hit : (Listof Object) Ray -> Boolean)
     ;; returns #t on the first Hit and #f if there are no hits
     ;;
     (define (find-hit objects ray)
       (match objects
         ['() #f]
         [(cons x rest) (if (Hit? ((ray-object-intersect ray) x))
                            #t
                            (find-hit rest ray))]))}
    (find-hit object-list ray)))

(check-expect (any-hit? sample-scene (make-ray (Vec3 0 0 1) (Vec3 0 1 0))) #t)
(check-expect (any-hit? sample-scene (make-ray (Vec3 1 14 0) (Vec3 0 -1 0))) #t)
(check-expect (any-hit? sample-scene (make-ray (Vec3 2 5 1) (Vec3 2 1 4))) #f)

;; An (Illum dir rgb) represents a source of light, where dir is a unit
;; vector pointing toward the light and rgb is the intensity of the light
;; at the point of illumination
(define-struct Illum
  ([dir : Vec3]
   [intensity : RGB]))

(: get-illumination : Scene Vec3 -> (Listof Illum))
;; returns a list of Illum values for the lights that cast illumination on the point
;;
(define (get-illumination scene vec)
  (local
    {(define scene-lights : (Listof Light) (Scene-lights scene))
     (: find-lights : (Listof Light) Vec3 (Listof Illum)-> (Listof Illum))
     ;; compares the ray made from p to light and finds lights that are not shadowed
     ;;
     (define (find-lights lights v illum)
       (match lights
         ['() illum]
         [(cons light rest) (if (any-hit? scene (make-ray v (Dir-Light-dir light)))
                                (find-lights rest v illum)
                                (find-lights rest
                                             v
                                             (cons (Illum (Dir-Light-dir light)
                                                          (Dir-Light-intensity light)) illum)))]))}
   (find-lights scene-lights vec '())))

(check-within (get-illumination sample-scene3 (Vec3 20 20 20))
              (list (Illum (Vec3 0.62017 0.24806 0.74420) (RGB 50 50 50))
                    (Illum (Vec3 1/3 2/3 -2/3) (RGB 0 1 0))
                    (Illum (Vec3 -1/3 2/3 -2/3) (RGB 1 0 0))) .001)
(check-within (get-illumination sample-scene3 (Vec3 10 7 0))
              (list (Illum (Vec3 0.62017 0.24806 0.74420) (RGB 50 50 50))
                    (Illum (Vec3 1/3 2/3 -2/3) (RGB 0 1 0))
                    (Illum (Vec3 -1/3 2/3 -2/3) (RGB 1 0 0))) .001)
(check-within (get-illumination sample-scene3 (Vec3 2.74 2.74 2.74))
              (list (Illum (Vec3 0.62017 0.24806 0.74420) (RGB 50 50 50))
                    (Illum (Vec3 1/3 2/3 -2/3) (RGB 0 1 0))
                    (Illum (Vec3 -1/3 2/3 -2/3) (RGB 1 0 0))) .0001)
(check-within (get-illumination sample-scene3 (Vec3 45.1919 -4.5192 0))
              (list (Illum (Vec3 0.62017 0.24806 0.74420) (RGB 50 50 50))
                    (Illum (Vec3 1/3 2/3 -2/3) (RGB 0 1 0))
                    (Illum (Vec3 -1/3 2/3 -2/3) (RGB 1 0 0))) 0.0001)

(: rgb-at : Scene Object Vec3 -> RGB)
;; takes the scene, an object, and a point of intersection on the object’s surface,
;; and returns the color at that point
;;
(define (rgb-at scene object v)
  (local
    {(define k : RGB (Material-diffuse (object-material object)))
     (define ambient : RGB (Scene-ambient scene))
     (define lights : (Listof Illum) (get-illumination scene v))
     (define n : Vec3 (object-normal object v))
     (: sum-lights : (Listof Illum) RGB -> RGB)
     ;; calculates the sum of the RGB values for the list of lights
     ;;
     (define (sum-lights illum color)
       (match illum
         ['() color]
         [(cons light rest)
          (if (= 0 (max (vec3-dot (Illum-dir light) n) 0))
                                (local {}
                                  (sum-lights rest color))
                                (local {}
                                  (sum-lights rest
                                              (rgb+ color
                                                    (rgb-scale (vec3-dot (Illum-dir light) n)
                                                                     (Illum-intensity light))))))]))}
    (rgb+ (rgb* k ambient) (rgb* k (sum-lights lights (RGB 0 0 0))))))

(check-within (rgb-at sample-scene3 sample-sphere (Vec3 45.1919 -4.5192 0))
              (RGB 579.71772 586.076642  579.61772) 0.0001)
(check-within (rgb-at sample-scene3
                      (make-sphere (Vec3 -1/3 1 6) 3/4 (Material (RGB 1 1 16/17)))
                      (Vec3 2.74 2.74 2.74))
              (RGB 3.178 5.910 0.0) 0.001)
(check-within (rgb-at sample-scene3 sample-sphere (Vec3 15 5 10))
              (RGB 356.69986 357.866528  356.59986) 0.0001)

(: trace-ray : Scene -> Ray -> RGB)
;; takes a Scene and the initial ray that defines a pixel and traces the ray
;; through the scene and computes the color of the light that flows
;; backwards along the ray’s path
;;
(define (trace-ray scene)
  (local
    {(: get-color : Ray -> RGB)
     ;; gets color from the intersection
     ;;
     (define (get-color ray)
       (match (closest-hit scene ray)
         ['miss (Scene-background scene)]
         [(Hit t obj) (local {} (rgb-at scene obj (ray-point-at ray t)))]))}
    get-color))

(check-expect ((trace-ray sample-scene2) (make-ray (Vec3 0 0 0) (Vec3 1 1 1)))
              (RGB 1 1 1))
(check-expect ((trace-ray sample-scene2) (make-ray (Vec3 0 0 0) (Vec3 1 1 1)))
              (RGB 1 1 1))
(check-expect ((trace-ray sample-scene) (make-ray (Vec3 2 5 1) (Vec3 2 1 4)))
              (RGB 50 50 50))
(check-within ((trace-ray sample-scene3) (make-ray (Vec3 0 0 0) (Vec3 10 -1 0)))
              (RGB 169/255 169/255 169/255) .0001)

(test)
 
;; Exports
;;;;;;;;;;

(provide Light
         Scene)

(provide make-dir-light
         make-scene
         trace-ray)