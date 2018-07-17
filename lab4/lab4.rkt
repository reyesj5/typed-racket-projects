#lang typed/racket

;; CMSC15100 Winter 2017
;; Labratory 4
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

;; a pixel location in an image
;;
(define-struct Point
  ([x : Integer]
   [y : Integer]))

(: point+ : Point Point -> Point)
;; adds two points and returns the resulting coordinate
;;
(define (point+ pt1 pt2)
  (Point (+ (Point-x pt1) (Point-x pt2)) (+ (Point-y pt1) (Point-y pt2))))
(check-expect (point+ (Point 2 5) (Point 5 1)) (Point 7 6))
(check-expect (point+ (Point -4 3) (Point 2 0)) (Point -2 3))
(check-expect (point+ (Point 0 0) (Point 7 9)) (Point 7 9))

(: point- : Point Point -> Point)
;; adds two points and returns the resulting coordinate
;;
(define (point- pt1 pt2)
  (Point (- (Point-x pt1) (Point-x pt2)) (- (Point-y pt1) (Point-y pt2))))
(check-expect (point- (Point 2 5) (Point 5 1)) (Point -3 4))
(check-expect (point- (Point -4 3) (Point 2 0)) (Point -6 3))
(check-expect (point- (Point 0 0) (Point 7 9)) (Point -7 -9))

;; A direction is clockwise rotation from the X axis
;; meatures in degrees and should be in the interval [0,360)
;;
(define-type Direction Real)

(: make-direction : Real -> Direction)
;; normalize a rotation angle to the interval [0,360).
;;
(define (make-direction theta)
  (local
    {(: inc : Real -> Real)
     ;; increment an angle by 360 until it is in the interval [0,360).
     ;;
     (define (inc theta)
       (if (< theta 0) (inc (+ theta 360)) theta))
     (: dec : Real -> Real)
     (define (dec theta)
       (if (<= 360 theta) (dec (- theta 360)) theta))}
     ;; decrement an angle by 360 until it is in the interval [0,360).
     ;;
    (cond
      [(< theta 0) (inc (+ theta 360))]
      [(<= 360 theta) (dec (- theta 360))]
      [else theta])))

(: direction-rotate : Direction Real -> Direction)
;; rotate the direction clockwise by the specified number of degrees
;; The result will be in the interval [0,360).
;;
(define (direction-rotate direction degrees)
  (make-direction (- direction (* -1 degrees))))
(check-expect (direction-rotate 90 60) 150)
(check-expect (direction-rotate 390 60) 90)
(check-expect (direction-rotate 90 100) 190)
(check-expect (direction-rotate 90 -60) 30)

(: move-point : Point Direction Integer -> Point)
;; returns the point that is at the given distance in the given direction
;;
(define (move-point point direction dist)
  (local
    {(define radians : Real (degrees->radians direction))
     (define x2 : Real (* dist (cos radians)))
     (define y2 : Real (* dist (sin radians)))}
    (point+ point (Point (exact-round x2) (exact-round y2)))))
(check-expect (move-point (Point 0 0) 30 1) (Point 1 0))
(check-expect (move-point (Point 2 5) 60 10) (Point 7 14))
(check-expect (move-point (Point 4 3) 45 15) (Point 15 14))
(check-expect (move-point (Point -5 -2) 0 9) (Point 4 -2))

;; a 2D drawing surface that supports negative coordinates
;;
(define-struct Surface
  ([offset : Point]     ;; value to subtract from points to account for negative coords
   [img : Image]))

(define empty-surface : Surface
  (Surface (Point 0 0) empty-image))

(: draw-line : Surface Point Point (U Image-Color Pen) -> Surface)
;; draw a line on the surface from p1 to p2 using the given color or pen
;;
(define (draw-line surf p1 p2 color-or-pen)
  (local
    {(define offset : Point (Surface-offset surf))
     ;; adjust points by current offset
     ;;
     (define q1 : Point (point- p1 offset))
     (define q2 : Point (point- p2 offset))
     ;; compute new offset
     ;;
     (define new-offset : Point
       (Point (min (Point-x offset) (Point-x p1) (Point-x p2))
              (min (Point-y offset) (Point-y p1) (Point-y p2))))}
    (Surface
      new-offset
      (add-line (Surface-img surf)
                (Point-x q1) (Point-y q1)
                (Point-x q2) (Point-y q2)
                color-or-pen))))

;; The representation of a turtle
;;
(define-struct Turtle
  ([pos : Point]                ;; the turtle's position
   [dir : Direction]            ;; the turtle's orientation
   [pen : (U Image-Color Pen)]  ;; the color/pen used to draw lines
   [surf : Surface]))           ;; the surface on which the turtle draws lines

(: make-turtle : (U Image-Color Pen) -> Turtle)
;; make a turtle with the given color/pen at the origin
;;
(define (make-turtle color-or-pen)
  (Turtle (Point 0 0) 0 color-or-pen empty-surface))
;; creating a turtle to testing the following functions
;;
(define myTurtle : Turtle (make-turtle 'red))

(: forward : Integer Turtle -> Turtle)
;; takes a distance and a turtle and draws a line in the current direction
;;
(define (forward dist turtle)
  (local
    {(define point2 : Point (move-point (Turtle-pos turtle)
                                        (Turtle-dir turtle)
                                        dist))}
    (Turtle point2
            (Turtle-dir turtle)
            (Turtle-pen turtle)
            (draw-line (Turtle-surf turtle)
                       (Turtle-pos turtle)
                       point2
                       (Turtle-pen turtle)))))
"forward test: moves turtle to the right by a distance of 500"
(Surface-img (Turtle-surf (forward 500 myTurtle)))
"forward test: moves turtle to the right by a distance of 250"
(Surface-img (Turtle-surf (forward 250 myTurtle)))

(: set-pen : Turtle (U Image-Color Pen) -> Turtle)
;; sets the color or pen of a given turtle by creating a new modified one
;;
(define (set-pen turtle color-or-pen)
  (Turtle (Turtle-pos turtle) (Turtle-dir turtle) color-or-pen (Turtle-surf turtle)))
"set-pen test: changes the color of a given turtle,
and moves it forward with the new color, blue"
(Surface-img (Turtle-surf (forward 250 (set-pen (forward 250 myTurtle) 'blue))))
"set-pen test: changes the color of a given turtle,
and moves it forward with the new color, black"
(Surface-img (Turtle-surf (forward 250 (set-pen (forward 250 myTurtle) 'black))))

(: turn : Direction Turtle -> Turtle)
;; rotates the turtle in a clockwise direction by the given angle
;;
(define (turn dir turtle)
  (Turtle (Turtle-pos turtle)
          (direction-rotate (Turtle-dir turtle) dir)
          (Turtle-pen turtle)
          (Turtle-surf turtle)))
"turn test: moves a turtle forward after rotating it 325 degrees clockwise"
(Surface-img (Turtle-surf (forward 250 (turn 325 myTurtle))))
"turn test: moves a turtle forward after rotating it 105 degrees clockwise"
(Surface-img (Turtle-surf (forward 250 (turn 105 myTurtle))))
"turn test: moves a turtle forward after rotating it 35 degrees clockwise"
(Surface-img (Turtle-surf (forward 250 (turn 35 myTurtle))))

(: move-to : Point Turtle -> Turtle)
;; moves the turtle to the given point without drawing anything
;;
(define (move-to point turtle)
  (Turtle point (Turtle-dir turtle) (Turtle-pen turtle) (Turtle-surf turtle)))
(check-expect (Turtle-pos (move-to (Point 25 5) myTurtle)) (Point 25 5))
(check-expect (Turtle-pos (move-to (Point 105 33) myTurtle)) (Point 105 33))
(check-expect (Turtle-pos (move-to (Point 52 66) myTurtle)) (Point 52 66))

(: move-by : Point Turtle -> Turtle)
;; moves the turtle by the relative offset without drawing anything
;;
(define (move-by point turtle)
  (local
    {(define point2 : Point (point+ (Turtle-pos turtle) point))}
    (move-to point2 turtle)))
(check-expect (Turtle-pos (move-by (Point 80 63) (move-to (Point 10 -53) myTurtle)))
              (Point 90 10))
(check-expect (Turtle-pos (move-by (Point -30 20) (move-to (Point 60 15) myTurtle)))
              (Point 30 35))
(check-expect (Turtle-pos (move-by (Point 10 -20) (move-to (Point 25 5) myTurtle)))
              (Point 35 -15))

(: get-image : Turtle -> Image)
;; returns the image that the turtle has drawn
;;
(define (get-image turtle)
  (Surface-img (Turtle-surf turtle)))
"get-image test: returns an image of a red and black line"
(get-image (forward 250 (set-pen (forward 250 myTurtle) 'black)))

(: polygon : Integer Integer (U Image-Color Pen) -> Image)
;; given the number of sides N, side-length D, and color/pen, draw an N-sided
;; polygon with sides of length D using the color/pen
(define (polygon N D color/pen)
  (local
    {(: polygon-helper : Turtle Integer Integer Integer -> Turtle)
     ;; returns a turtle after moving forward distance D until i = N
     ;;
     (define (polygon-helper turtle i n d)
       (if (< n i)
           turtle
           (polygon-helper (turn (/ 360 n) (forward d turtle)) (+ i 1) n d)))}
    (if (or (< N 3) (< D 1))
        (error "polygon: polygon must be at least length 1 and have at least 3 sides")
        (get-image (polygon-helper (make-turtle color/pen) 1 N D)))))
"polygon test: Draws a pentagon"
(polygon 5 50 'blue)
"polygon test: Draws a triangle"
(polygon 3 50 'black)
"polygon test: Draws an octagon"
(polygon 8 50 'red)
(check-error (polygon 8 0 'red)
             "polygon: polygon must be at least length 1 and have at least 3 sides")
(check-error (polygon 2 69 'red)
             "polygon: polygon must be at least length 1 and have at least 3 sides")

;; run tests
;;
(test)