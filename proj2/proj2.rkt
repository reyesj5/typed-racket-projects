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

;; load objects module
;;
(require "objects.rkt")

;; load scene module
;;
(require "scene.rkt")

;; load camera module
;;
(require "camera.rkt")

(: render : Camera Scene Natural -> Image)
;; produces an image for a given camera and scene
;;
(define (render camera scene depth)
  (foreach-pixel camera (trace-ray scene depth)))

;; Colors
(define rgb:white      (RGB 1 1 1))
(define rgb:black      (RGB 0 0 0))
(define rgb:gray       (RGB 38/51 38/51 38/51))
(define rgb:darkgray   (RGB 169/255 169/255 169/255))
(define rgb:red        (RGB 1 0 0))
(define rgb:green      (RGB 0 1 0))
(define rgb:blue       (RGB 0 0 1))
(define rgb:pink       (RGB 1 64/85 203/255))
(define rgb:silver     (RGB 64/85 64/85 64/85))
(define rgb:ivory      (RGB 1 1 16/17))
(define rgb:orange     (RGB 1 11/17 0))
(define rgb:dodgerblue (RGB 2/17 48/85 1))
(define rgb:skyblue    (RGB 9/17 206/255 47/51))
(define rgb:navy       (RGB 36/255 36/255 140/255))

;; Materials
(define mat:orange (make-diffuse rgb:orange))
(define mat:pink (make-diffuse rgb:pink))
(define mat:dodgerblue (make-diffuse rgb:dodgerblue))
(define mat:silver (make-diffuse rgb:silver))
(define mat:ivory (make-diffuse rgb:ivory))
(define mat:gray (make-diffuse rgb:gray))
(define mat:skyblue (make-diffuse rgb:skyblue))

;; Cameras (4:3 aspect ratio)
(define cs151-camera-1 (Camera 200 150 -5 5))
(define cs151-camera-2 (Camera 200 150 -8 8))
(define cs151-camera-2a (Camera 200 150 -8 4))
(define cs151-camera-2b (Camera 200 150 -4 4))
(define cs151-camera-3 (Camera 480 320 -8 8))

(define cs151-test-scene-1
  (Scene
   rgb:darkgray
   (list (make-sphere (Vec3 0 0 3) 1 mat:orange))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-2
  (Scene
   rgb:darkgray
   (list (make-sphere (Vec3 0 0 6) 1 mat:orange))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-3
  (Scene
   rgb:navy
   (list (make-sphere (Vec3 0 0 6) 1 mat:pink))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-4
  (Scene
   rgb:navy
   (list
    (make-sphere (Vec3  3/2 0 8) 1 mat:dodgerblue)
    (make-sphere (Vec3 -3/2 0 8) 1 mat:dodgerblue))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-5
  (Scene
   rgb:black
   (list
    (make-sphere (Vec3  3/2 0 8)  1 mat:dodgerblue)
    (make-sphere (Vec3 -3/2 0 8)  1 mat:dodgerblue)
    (make-sphere (Vec3    0 0 20) 1 mat:silver))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-6
  (Scene
   rgb:black
   (list
    (make-sphere (Vec3    1  -1  8) 1 mat:dodgerblue)
    (make-sphere (Vec3   -1   1  8) 1 mat:dodgerblue))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-7
  (Scene
   rgb:black
   (list
    (make-sphere (Vec3    1    -1  8) 2 mat:ivory)
    (make-sphere (Vec3   -1/3   1  5) 3/4 mat:ivory))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

(define cs151-test-scene-8
  (Scene
   rgb:darkgray
   (list
    (make-sphere (Vec3    1    -1  9) 2  mat:ivory)
    (make-sphere (Vec3   -1/3   1  6) 3/4  mat:ivory))
   (RGB 0.2 0.0 0.0)
   (list (make-dir-light (Vec3 -1/2 1 -1) rgb:red))))

(define cs151-test-scene-9
  (Scene
   rgb:darkgray
   (list
    (make-sphere (Vec3    1    -1  9) 2  mat:ivory)
    (make-sphere (Vec3   -1/3   1  6) 3/4  mat:ivory))
   (RGB 0.0 0.2 0.0)
   (list (make-dir-light (Vec3  1/2 1 -1) rgb:green))))

(define cs151-test-scene-10
  (Scene
   rgb:darkgray
   (list
    (make-sphere (Vec3    1    -1  9) 2  mat:ivory)
    (make-sphere (Vec3   -1/3   1  6) 3/4  mat:ivory))
   (RGB 0.1 0.1 0.0)
   (list (make-dir-light (Vec3 -1/2 1 -1) rgb:red)
         (make-dir-light (Vec3  1/2 1 -1) rgb:green))))

(define cs151-test-scene-11
  (Scene
   rgb:black
   (list
    (make-sphere (Vec3 1 1 8) 2/3 mat:gray)
    (make-sphere (Vec3 0 0 1) 1/2 mat:skyblue))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) rgb:white))))

;; Project 2 Tests

;; examples part 1
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
"testing render: produces orange ball on grey/green background"
(render (Camera 320 256 -8 4) scene-1 1)

(define scene-2
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 1 0) -1 mat-green))
   (RGB 0.2 0.2 0.2)
   (list (make-point-light (Vec3 0 1 3) (RGB 1 1 1) 0.05))))
"testing render with point lights: produces shiny green bottom half"
(render (Camera 320 256 -8 4) scene-2 1)

;; examples part 2
;;
(: make-test-scene : Real -> Scene)
(define (make-test-scene sharpness)
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-sphere (Vec3 0 -2 1) 1 (Material (RGB 0.5 0.1 0.5) (RGB 1 1 1) sharpness)))
   (RGB 0.3 0.2 0.3)
   (list
    (make-dir-light (Vec3 0 1 6) (RGB 1 1 1)))))

(: specular-test : Real -> Image)
(define (specular-test sharpness)
  (render (Camera 320 256 -5 2) (make-test-scene sharpness) 0))
"testing specular lighting: Displays test image 1"
(specular-test 1)
"testing specular lighting: Displays test image 2"
(specular-test 10)
"testing specular lighting: Displays test image 3"
(specular-test 20)

;; examples part 3
(define mat-dodgerblue (make-diffuse (RGB 2/17 48/85 1)))
(define mat-mirror (Material (RGB 0.1 0.1 0.1) (RGB 1 1 1) 10))

(define flat-mirror-scene
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 0 -1) -10 mat-mirror)
    (make-plane (Vec3 0 1 0) -1 mat-mirror)
    (make-sphere (Vec3 -1.5 0.5 5) 1/2 mat-orange)
    (make-sphere (Vec3 1.5 0.5 5) 1/2 mat-dodgerblue))
   (RGB 1 1 1)
   (list)))

(: flat-mirror-test : Natural -> Image)
(define (flat-mirror-test depth-limit)
  (render (Camera 320 256 -8 4) flat-mirror-scene depth-limit))

"testing mirrors: Displays test image 1"
(flat-mirror-test 0)
"testing mirrors: Displays test image 2"
(flat-mirror-test 1)
"testing mirrors: Displays test image 3"
(flat-mirror-test 2)

(define mirror-scene
  (make-scene
   (RGB 169/255 169/255 169/255)
   (list
    (make-plane (Vec3 0 0 -1) -10 mat-mirror)
    (make-plane (Vec3 0 1 0) -1 mat-mirror)
    (make-sphere (Vec3 -1.5 0.5 5) 1/2 mat-orange)
    (make-sphere (Vec3 1.5 0.5 5) 1/2 mat-dodgerblue))
   (RGB 0.2 0.2 0.2)
   (list (make-dir-light (Vec3 -1 1 -1) (RGB 0.8 0.8 0.8)))))

(: mirror-test : Natural -> Image)
(define (mirror-test depth-limit)
  (render (Camera 320 256 -8 4) mirror-scene depth-limit))

"testing directional light: Displays test image 4"
(mirror-test 2)

;; Project 1 Tests
;;

;"multiple scenes for testing render dunction"
;(render cs151-camera-1 cs151-test-scene-1)
;(render cs151-camera-2 cs151-test-scene-1)
;(render cs151-camera-2a cs151-test-scene-1)
;(render cs151-camera-2b cs151-test-scene-1)
;(render cs151-camera-3 cs151-test-scene-1)
;(render cs151-camera-1 cs151-test-scene-2)
;(render cs151-camera-2 cs151-test-scene-2 10)
;(render cs151-camera-2a cs151-test-scene-2)
;(render cs151-camera-2b cs151-test-scene-2)
;(render cs151-camera-3 cs151-test-scene-2)
;(render cs151-camera-1 cs151-test-scene-3)
;(render cs151-camera-2 cs151-test-scene-3)
;(render cs151-camera-2a cs151-test-scene-3 10)
;(render cs151-camera-2b cs151-test-scene-3)
;(render cs151-camera-3 cs151-test-scene-3)
;(render cs151-camera-1 cs151-test-scene-4)
;(render cs151-camera-2 cs151-test-scene-4)
;(render cs151-camera-2a cs151-test-scene-4)
;(render cs151-camera-2b cs151-test-scene-4)
;(render cs151-camera-3 cs151-test-scene-4 10)
;(render cs151-camera-1 cs151-test-scene-5)
;(render cs151-camera-2 cs151-test-scene-5)
;(render cs151-camera-2a cs151-test-scene-5)
;(render cs151-camera-2b cs151-test-scene-5)
;(render cs151-camera-3 cs151-test-scene-5)
;(render cs151-camera-1 cs151-test-scene-6)
;(render cs151-camera-2 cs151-test-scene-6 10)
;(render cs151-camera-2a cs151-test-scene-6)
;(render cs151-camera-2b cs151-test-scene-6)
;(render cs151-camera-3 cs151-test-scene-6)
;(render cs151-camera-1 cs151-test-scene-7)
;(render cs151-camera-2 cs151-test-scene-7)
;(render cs151-camera-2a cs151-test-scene-7 10)
;(render cs151-camera-2b cs151-test-scene-7)
;(render cs151-camera-3 cs151-test-scene-7)
;(render cs151-camera-1 cs151-test-scene-8)
;(render cs151-camera-2 cs151-test-scene-8)
;(render cs151-camera-2a cs151-test-scene-8)
;(render cs151-camera-2b cs151-test-scene-8 10)
;(render cs151-camera-3 cs151-test-scene-8)
;(render cs151-camera-1 cs151-test-scene-9)
;(render cs151-camera-2 cs151-test-scene-9)
;(render cs151-camera-2a cs151-test-scene-9)
;(render cs151-camera-2b cs151-test-scene-9)
;(render cs151-camera-3 cs151-test-scene-9)
;(render cs151-camera-1 cs151-test-scene-10 10)
;(render cs151-camera-2 cs151-test-scene-10)
;(render cs151-camera-2a cs151-test-scene-10)
;(render cs151-camera-2b cs151-test-scene-10)
;(render cs151-camera-3 cs151-test-scene-10)
;(render cs151-camera-1 cs151-test-scene-11 10)
;(render cs151-camera-2 cs151-test-scene-11)
;(render cs151-camera-2a cs151-test-scene-11)
;(render cs151-camera-2b cs151-test-scene-11)
;(render cs151-camera-3 cs151-test-scene-11)

(test)