#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 1 -- Material module
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

;; three-channel representation of light
;;
(define-struct RGB
  ([r : Real]       ;; r is a real value representing the intensity of the red channel
   [g : Real]       ;; g is a real value representing the intensity of the green channel
   [b : Real]))     ;; b is a real value representing the intensity of the blue channel

(: rgb+ : RGB RGB -> RGB)
;; Add two RGB values
;;
(define (rgb+ color1 color2)
  (RGB (+ (RGB-r color1) (RGB-r color2))
       (+ (RGB-g color1) (RGB-g color2))
       (+ (RGB-b color1) (RGB-b color2))))

(check-within (rgb+ (RGB .26 .61 .20) (RGB .64 .12 .31)) (RGB .9 .73 .51) .00001)
(check-within (rgb+ (RGB 0 .2 .51) (RGB .64 .11 .31)) (RGB .64 .31 .82) .00001)
(check-within (rgb+ (RGB .54 .6 .05) (RGB .14 .12 .31)) (RGB .68 .72 .36) .00001)

(: rgb* : RGB RGB -> RGB)
;; Pointwise multiplication of two RGB values
;;
(define (rgb* color1 color2)
  (RGB (* (RGB-r color1) (RGB-r color2))
       (* (RGB-g color1) (RGB-g color2))
       (* (RGB-b color1) (RGB-b color2))))

(check-within (rgb* (RGB .26 .61 .20) (RGB .64 .12 .31)) (RGB .1664 .0732 .062) .00001)
(check-within (rgb* (RGB 0 .2 .51) (RGB .64 .11 .31)) (RGB 0 .022 .1581) .00001)
(check-within (rgb* (RGB .54 .6 .05) (RGB .14 .12 .31)) (RGB .0756 .072 .0155) .00001)

(: rgb-scale : Real RGB -> RGB)
;; Multiply a scalar and an RGB value
;;
(define (rgb-scale scalar color2)
  (RGB (* scalar (RGB-r color2))
       (* scalar (RGB-g color2))
       (* scalar (RGB-b color2))))

(check-within (rgb-scale 2 (RGB .44 .12 .31)) (RGB .88 .24 .62) .00001)
(check-within (rgb-scale 0 (RGB .64 .11 .31)) (RGB 0 0 0) .00001)
(check-within (rgb-scale 3 (RGB .14 .12 .31)) (RGB .42 .36 .93) .00001)

(: rgb->color : RGB -> Color)
;; convert an RGB value to an image color value
(define (rgb->color rgb)
  (local
    {(: ->byte : Real -> Byte)
     (define (->byte x)
       (local
         {(define b (exact-floor (* 255.99 x)))}
         (cond
           [(< b 0) 0]
           [(< 255 b) 255]
           [else b])))}
    (match rgb
      [(RGB r g b) (color (->byte r) (->byte g) (->byte b))])))

(check-expect (rgb->color (RGB 0 0 0)) (color 0 0 0))
(check-expect (rgb->color (RGB 1 0 0)) (color 255 0 0))
(check-expect (rgb->color (RGB 0 1 0)) (color 0 255 0))
(check-expect (rgb->color (RGB 0 0 1)) (color 0 0 255))


;; restricting materials to be simple diffuse surfaces
;;
(define-struct Material
  ([diffuse : RGB]))

(test)

;; Exports
;;;;;;;;;;

(provide (struct-out RGB)
         (struct-out Material))

(provide rgb+
         rgb*
         rgb-scale
         rgb->color)