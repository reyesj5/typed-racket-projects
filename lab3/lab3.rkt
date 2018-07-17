#lang typed/racket

;; CMSC15100 Winter 2017
;; Labratory 3
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

;; the length of a trinagle's edge
;;
(define tri-side : Integer 30)

;; a Pen for drawing the border of a triangle
;;
(define border-pen : Pen (pen "black" 1 "solid" "round" "round"))

(: tile : Image-Color -> Image)
;; draw a triangular tile of a given color
;;
(define (tile c)
  (overlay
   (triangle tri-side 'outline border-pen)
   (triangle tri-side 'solid c)))
"Test: draws a triangle tile"
(tile "silver")

(: center-x : Image -> Integer)
;; computes the x-coordinate for the center of an image
;;
(define (center-x image)
  (floor (/ (image-width image) 2)))
(check-expect (center-x (tile "silver")) 15)
  
(: center-y : Image -> Integer)
;; computes the x-coordinate for the center of an image
;;
(define (center-y image)
  (floor (/ (image-height image) 2)))
(check-expect (center-y (tile "silver")) 13)

(: above-offset : Image Real Real Image -> Image)
;; place image i1 above image i2 and then adjust the position
;; of i2 by (x-offset, y-offset).  If the images overlap, then
;; i1 will be in front of i2
;;
(define (above-offset i1 x-offset y-offset i2)
  (overlay/offset i1 x-offset
   (+ y-offset (center-y i1) (center-y i2))
   i2))
"Testing above-offset: draws two triangles of two colors,
red and silver"
(above-offset (tile "red") 0 0 (tile "silver"))

(: diamond : Image -> Image)
;; creates a diamond form two tringles of the same color
;;
(define (diamond image)
  (above-offset image 0 0 (flip-vertical image)))
"Testing diamond: draws a diamond from one triangle"
(diamond (tile "red"))

(: tile-pair : Image Integer -> Image)
;; aligns two diamonds horizontally with the centers separated
;; by a multiple of width
;;
(define (tile-pair image m)
  (overlay/offset image (* m 30) 0 image))
"Testing tile-pair: draws a tile-pair"
(tile-pair (diamond (tile "red")) 1)
"Testing tile-pair: draws a diamond"
(tile-pair (diamond (tile "red")) 0)
"Testing tile-pair: draws a tile-pair separated by 3 multiples"
(tile-pair (diamond (tile "red")) 3)

(: draw-v : Image Integer -> Image)
;; draws a "V" given a diamond and the inside dimension of the "V"
(define (draw-v image n)
  (if (= n 0) image (above-offset (tile-pair image n) 0 -26
                                  (draw-v image (- n 1)))))
"Testing draw-v: draws a v of dimension 3"
(draw-v (diamond (tile "red")) 3)
"Testing draw-v: draws a v of dimension 0"
(draw-v (diamond (tile "red")) 0)

(: add-v : Image Image -> Image)
;; adds the next "v" image2 to a diamond image1
;;
(define (add-v image1 image2)
  (above-offset image1 0 (* -.5 (image-height image1)) image2))
"Testing add-v: draws an arm of dimension 1"
(add-v (diamond (tile "red")) (draw-v (diamond (tile "red")) 1))
"Testing add-v: draws an arm of dimension 2"
(add-v (add-v (diamond (tile "red"))
              (draw-v (diamond (tile "red")) 1))
       (draw-v (diamond (tile "red")) 2))

(: draw-arm : Integer Image Image -> Image)
;; draws an arm of the star of n dimension from the given diamonds
;;
(define (draw-arm n image1 image2)
  (cond
    [(< n 0) (error "draw-arm: arm dimension must be greater than 0")]
    [(= n 0) image1]
    [(= 0 (remainder n 2))
     (add-v (draw-arm (- n 1) image1 image2) (draw-v image1 n))]
    [else (add-v (draw-arm (- n 1) image1 image2)
                 (draw-v image2 n))]))
"Testing draw-arm: draws an arm of dimension 4"
(draw-arm 4 (diamond (tile "crimson")) (diamond (tile "silver")))

(: rotate-arm : Integer Integer Image -> Image)
;; rotates the arm n times by m degrees and returns the overlay
;;
(define (rotate-arm n m image1)
  (cond
    [(= n 0) image1]
    [else (overlay/pinhole
           image1 (rotate-arm (- n 1) m (rotate m image1)))]))
"Testing rotate-arm: Rotates an arm 4 times by 60 degrees"
(rotate-arm 4 60 (put-pinhole 45 0 (draw-arm 2 (diamond (tile "blue"))
                                               (diamond (tile "yellow")))))

(: draw-star : Integer Image-Color Image-Color -> Image)
;; draw a six-pointed star mosaic using triangular tiles, where the first
;; argument is the inner radius of the star and the other two arguments
;; are the alternating colors of the star.
;;
(define (draw-star n color1 color2)
  (: image1 : Image)
  ;; creates an arm of the star with given colors and n dimension
  (define image1 (draw-arm (- n 1)
                           (diamond (tile color1))
                           (diamond (tile color2))))
  (cond
    [(<= n 0) (error "draw-star: star radius must be greater than 0")]
    [else
     (clear-pinhole
      (rotate-arm 5 60 (put-pinhole (center-x image1) 0 image1)))]))
"Testing draw-star: Draws a star of radius 4"
(draw-star 4 "red" "silver")
"Testing draw-star: Draws a star of radius 10"
(draw-star 8 "black" "purple")
"Testing draw-star: Draws a star of radius 25"
(draw-star 16 "yellow" "black")

;; run tests
;;
(test)