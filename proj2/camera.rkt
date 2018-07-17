#lang typed/racket

;; CMSC15100 Winter 2017
;; Project 1 -- camera.rkt module
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

;; A (Camera w h z flen) specifies the size of the view image and
;; the viewing parameters
(define-struct Camera
  [(wid : Natural)           ;; width of view image
   (ht : Natural)            ;; height of view image
   (z-pos : Real)            ;; z-coordinate of camera
   (focal-len : Real)])      ;; distance to image plane

(: ray-for-pixel : Camera -> (Integer Integer -> Ray))
;; takes a camera and returns a function for generating rays from a row and column
;;
(define (ray-for-pixel camera)
  (local
    {(define pw : Real (/ 2 (Camera-wid camera)))
     (define view-z : Real (+ (Camera-z-pos camera) (Camera-focal-len camera)))
     (define x0 : Real (- (/ pw 2) 1))
     (define y0 : Real (- (/ (Camera-ht camera) (Camera-wid camera)) (/ pw 2)))
     (: get-rays : Integer Integer -> Ray)
     ;; generates rays from a row and column
     ;;
     (define (get-rays r c)
       (local
         {(define pixel-origin : Vec3 (Vec3 (+ x0 (* c pw)) (- y0 (* r pw)) view-z))
          (define camera-position : Vec3 (Vec3 0 0 (Camera-z-pos camera)))}
         (make-ray pixel-origin
                   (vec3-negate (vec3- camera-position pixel-origin)))))}
    get-rays))

(check-within ((ray-for-pixel (Camera 100 50 25 10)) 10 20)
              (Ray (Vec3 -59/100 29/100 35)  (Vec3 -0.05887 0.02893 0.99784)) 0.0001)
(check-within ((ray-for-pixel (Camera 50 300 14 11)) 8 3)
              (Ray (Vec3 -43/50 283/50 25)  (Vec3 -0.06935 0.45642 0.88705)) 0.0001)
(check-within ((ray-for-pixel (Camera 200 150 25 10)) 15 20)
              (Ray (Vec3 -159/200 119/200 35) (Vec3 -0.07911 0.059208 0.995105)) 0.0001)

(: foreach-pixel : Camera (Ray -> RGB) -> Image)
;; iterates over the pixels in the image rectangle producing RGB values
;; The RGB values will be converted to colors and assembled into a list
;;
(define (foreach-pixel camera get-color)
  (local
    {(define row : Integer (Camera-wid camera))
     (define col : Integer (Camera-ht camera))
     (: process-pixels : Integer Integer -> (Listof Color))
     ;; goes through a row and gets the color for each pixel
     ;;
     (define (process-pixels r c)
       (if (= c col)
           '()
           (if (= row r)
               (process-pixels 0 (+ c 1))
               (cons (rgb->color (get-color ((ray-for-pixel camera) c r)))
                     (process-pixels (+ r 1) c)))))}
    (color-list->bitmap (process-pixels 0 0)
                        (Camera-wid camera)
                        (Camera-ht camera))))

"foreach-pixel test: produces the first image given by instructor"
(foreach-pixel
 (Camera 128 128 -5 5.25)
 (lambda ([ray : Ray])
   (RGB (* 0.5 (+ (Vec3-x (Ray-origin ray)) 1))
        (* 0.5 (+ (Vec3-y (Ray-origin ray)) 1))
        (Vec3-z (Ray-origin ray)))))

"foreach-pixel test: produces the second image given by instructor"
(foreach-pixel
 (Camera 128 128 -1 0.5)
 (lambda ([ray : Ray])
   (RGB (* 0.5 (+ (Vec3-x (Ray-dir ray)) 1))
        (* 0.5 (+ (Vec3-y (Ray-dir ray)) 1))
        (* 0.5 (+ (Vec3-z (Ray-dir ray)) 1)))))

(test)

;; Exports
;;;;;;;;;;

(provide (struct-out Camera))

(provide foreach-pixel)