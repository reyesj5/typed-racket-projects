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

;; a point-light in the scene that emits light in all directions
;;
(define-struct Point-Light
  ([pos : Vec3]
   [intensity : RGB]
   [atten : Real]))

(: make-point-light : Vec3 RGB Real -> Light)
;; makes a point-light
(define make-point-light Point-Light)

;; generic type of lights
;;
(define-type Light (U Dir-Light Point-Light))

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

;; sample scenes
;;
(define mat-green (make-diffuse (RGB 0 1 0)))
(define mat-orange (make-diffuse (RGB 1 11/17 0)))

(define scene-1
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 1 0) -2 mat-green)
    (make-sphere (Vec3 0 0 3) 1 mat-orange))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) (RGB 1 1 1)))))
(define scene-2
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 1 0) -2 mat-green))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) (RGB 1 1 1)))))
(define scene-3
  (make-scene
   (RGB 169/255 169/255 169/255)
   '()
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) (RGB 1 1 1)))))

;; sample sphere to test functions
;;
(define sample-sphere : Object (make-sphere (Vec3 2 1 3) 2 (make-diffuse (RGB 1 1 1))))

;; sample scenes to test functions
;;
(define sample-scene : Scene
  (make-scene (RGB 50 50 50)
              (list (make-sphere (Vec3 0 0 0) 2 (make-diffuse (RGB 1 1 1)))
                    (make-sphere (Vec3 5 20 0) 1 (make-diffuse (RGB 1 1 1)))
                    (make-sphere (Vec3 0 10 6) 5 (make-diffuse (RGB 1 1 1))))
              (RGB 7 11 13)
              (list (make-dir-light (Vec3 2 1 5) (RGB 25 30 41)))))
(define sample-scene2 : Scene
  (make-scene (RGB 1 1 1)
              (list (make-sphere (Vec3 10 10 10) 20 (make-diffuse (RGB 1 1 1))))
              (RGB 10 50 90)
              (list (make-dir-light (Vec3 1 1 1) (RGB 1 1 1)))))
(define sample-scene3 : Scene
  (make-scene (RGB 169/255 169/255 169/255)
              (list (make-sphere (Vec3 1 -1 9) 2 (make-diffuse (RGB 1 1 16/17)))
                    (make-sphere (Vec3 -1/3 1 6) 3/4 (make-diffuse (RGB 1 1 16/17))))
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
(check-expect (nearest-hit (Hit 2 sample-sphere) (Hit 5 sample-sphere))
              (Hit 2 sample-sphere))

(: closest-hit : Scene Ray -> Maybe-Hit)
;; takes a Scene and the initial ray that defines a pixel and traces the ray through the
;; scene and computes the color of the light that flows backwards along the ray’s path
;;
(define (closest-hit scene ray)
  (local
    {(define object-list : (Listof Object) (Scene-objects scene))}
    (foldl (lambda ([x : Object] [nearest : Maybe-Hit])
             (nearest-hit nearest ((ray-object-intersect ray) x)))
           'miss object-list)))

(check-within (closest-hit sample-scene (make-ray (Vec3 3 3 5) (Vec3 0 1 0)))
              (Hit 3.12701 (make-sphere (Vec3 0 10 6) 5 (make-diffuse (RGB 1 1 1))))
              0.0001)
(check-within (closest-hit sample-scene (make-ray (Vec3 1 14 0) (Vec3 0 -1 0)))
              (Hit 12.26794 (make-sphere (Vec3 0 0 0) 2 (make-diffuse (RGB 1 1 1))))
              0.0001)
(check-expect (closest-hit sample-scene (make-ray (Vec3 2 5 1) (Vec3 2 1 4))) 'miss)
(check-expect (closest-hit sample-scene2 (make-ray (Vec3 0 0 0) (Vec3 1 1 1))) 'miss)
(check-expect (closest-hit sample-scene2 (make-ray (Vec3 0 0 0) (Vec3 4 5 6))) 'miss)

(: any-hit? : Scene Ray Real -> Boolean)
;; returns #t on the first Hit and #f if there are no hits
;;
(define (any-hit? scene ray max-d)
  (local
    {(define object-list : (Listof Object) (Scene-objects scene))}
    (ormap (lambda ([x : Object])
             (local
               {(define hit : Maybe-Hit ((ray-object-intersect ray) x))}
               (if (Hit? hit)
                   (< (Hit-t hit) max-d)
                   #f)))
           object-list)))
(check-expect (any-hit? scene-1 (make-ray (Vec3 0 0 0) (Vec3 0 1 3)) 1000) #t)
(check-expect (any-hit? sample-scene (make-ray (Vec3 0 0 1) (Vec3 0 1 0)) +inf.0) #t)
(check-expect (any-hit? sample-scene (make-ray (Vec3 1 14 0) (Vec3 0 -1 0)) +inf.0) #t)
(check-expect (any-hit? sample-scene (make-ray (Vec3 2 5 1) (Vec3 2 1 4)) +inf.0) #f)

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
     (: check-light : Light (Listof Illum) -> (Listof Illum))
     ;;
     ;;
     (define (check-light light list)
       (match light
         [(Dir-Light dir intensity) (if (any-hit? scene
                                                  (ray-offset (make-ray vec dir) .001)
                                                  +inf.0)
                                        list
                                        (cons (Illum dir intensity) list))]
         [(Point-Light pos intensity a)
          (local {(define dir : Vec3 (vec3- pos vec))
                  (define dist : Real (vec3-length dir))
                  (define unit-dir : Vec3 (vec3-normalize dir))} 
            (if (any-hit? scene (ray-offset (make-ray pos unit-dir) .001) dist)
                list
                (cons (Illum unit-dir intensity) list)))]))}
    (foldl (lambda ([light : Light] [illum : (Listof Illum)])
             (check-light light illum))
           '()
           scene-lights)))

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

;; Poject 2
;;

(: diffuse-rgb : Scene Material Vec3 (Listof Illum) -> RGB)
;; given the surface material, surface normal, and list of light sources,
;; compute the ambient and diffuse components of the surface color
;;
(define (diffuse-rgb scene surf nvec illums)
  (local
    {(define diffuse (Material-diffuse surf))
     (: do-illum : Illum RGB -> RGB)
     ;; add the diffuse light from one source
     (define (do-illum illum rgb)
       (rgb+ (rgb-scale
              (max 0 (vec3-dot (Illum-dir illum) nvec))
              (Illum-intensity illum))
             rgb))}
    (rgb* diffuse (foldl do-illum (Scene-ambient scene) illums))))

(check-within (diffuse-rgb sample-scene3
                           (object-material sample-sphere)
                           (Vec3 45.1919 -4.5192 0)
                           (get-illumination sample-scene3 (Vec3 5 5 5)))
              (RGB 1345.38755 1357.43871 1345.28755) 0.0001)

(: specular-rgb : Material Vec3 Vec3 (Listof Illum) -> RGB)
;; returns the specular-lighting component of the illumination equation
;;
(define (specular-rgb mat nvec uvec lights)
  (local
    {(define specular (Material-spec mat))
     (: do-illum : Illum RGB -> RGB)
     ;; add the diffuse light from one source
     (define (do-illum illum rgb)
       (rgb+ (rgb-scale
              (expt (max 0 (vec3-dot (vec3-halfway (Illum-dir illum) uvec) nvec))
                    (Material-sharp mat))
              (Illum-intensity illum))
             rgb))}
    (rgb* specular (foldl do-illum (RGB 0 0 0) lights))))

(: trace : Scene Ray Integer -> RGB)
;; trace a ray in the scene, where the third argument specifies
;; the recursion-depth limit.  If it is negative, then return the
;; black.  Otherwise check for a hit and compute the ray's color
;; if there is a hit.  If there is no hit, then return the
;; background color.
(define (trace scene ray depth-limit)
  (local
    {(: get-color : Ray Integer -> RGB)
     ;; recursiveley traces a ray to n deph
     ;;
     (define (get-color ray n)
       (if (< n 0)
           (RGB 0 0 0)
           (match (closest-hit scene ray)
             ['miss (Scene-background scene)]
             [(Hit t obj)
              (local
                {(define mat : Material (object-material obj))
                 (define point : Vec3 (ray-point-at ray t))
                 (define illums : (Listof Illum) (get-illumination scene point))
                 (define normal : Vec3 (object-normal obj point))}
                (if (specular? mat)
                    (rgb+ (diffuse-rgb scene mat normal illums)
                          (rgb+ (specular-rgb mat
                                              normal
                                              (vec3-negate (Ray-dir ray))
                                              illums)
                                (rgb* (Material-spec mat)
                                      (get-color (ray-offset
                                                  (make-ray point
                                                            (vec3-reflect (Ray-dir ray)
                                                                          normal))
                                                  .001)
                                                 (- n 1)))))
                    (diffuse-rgb scene mat normal illums)))])))}
    (get-color ray depth-limit)))

(: trace-ray : Scene Natural -> Ray -> RGB)
;; takes a Scene and the initial ray that defines a pixel and traces the ray
;; through the scene and computes the color of the light that flows
;; backwards along the ray’s path
;;
(define (trace-ray scene depth)
  (local
    {(: get-color : Ray -> RGB)
     ;; gets color from the intersection
     ;;
     (define (get-color ray)
       (trace scene ray depth))}
    get-color))

(check-expect ((trace-ray sample-scene2 1) (make-ray (Vec3 0 0 0) (Vec3 1 1 1)))
              (RGB 1 1 1))
(check-expect ((trace-ray sample-scene2 1) (make-ray (Vec3 0 0 0) (Vec3 1 1 1)))
              (RGB 1 1 1))
(check-expect ((trace-ray sample-scene 1) (make-ray (Vec3 2 5 1) (Vec3 2 1 4)))
              (RGB 50 50 50))
(check-within ((trace-ray sample-scene3 1) (make-ray (Vec3 0 0 0) (Vec3 10 -1 0)))
              (RGB 169/255 169/255 169/255) .0001)

(test)
 
;; Exports
;;;;;;;;;;

(provide Light
         Scene)

(provide make-dir-light
         make-scene
         trace-ray)

;; Project 2 Exports
;;;;;;;;;;;;;;;;;;;;

(provide make-point-light)