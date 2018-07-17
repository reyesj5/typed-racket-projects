#lang typed/racket

;; CMSC15100 Winter 2017
;; Labratory 5
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

;; load turtle library
;;
(require "turtle.rkt")

;; An LSym (L-system symbol) is either a Turn operation, or a Symbol
;; The following symbols have special interpretation:
;;    '<<     -- push/save turtle state
;;    '>>     -- pop/restore turtle state
;;    'F, 'f  -- forward
;;
(define-type LSym (U Turn Symbol))

;; the Turn command rotates the turtle by some angle (in degrees)
;;
(define-struct Turn
  ([angle : Real]))

;; An L-system string is a list of L-system symbols
;;
(define-type LString (Listof LSym))

;; An L-system rule describes an expansion 'lhs --> rhs'
;;
(define-struct LRule
  ([lhs : Symbol]    ;; left-hand-side symbol to expand
   [rhs : LString])) ;; right-hand-side string to substitute for the lhs symbol

;; An L-system is represented as an initial string and a list of rules
;;
(define-struct LSystem
  ([initial : LString]
   [rules : (Listof LRule)]))

(: lsys-expand-symbol : LSystem Symbol -> LString)
;; Given an L-System and a Symbol, return the expansion of the symbol.
;; Return a singleton list of the symbol itself if it is not the
;; left-hand-side of any rule in the system
;;
(define (lsys-expand-symbol system symbol)
  (local
    {(: expand-rule : (Listof LRule) Symbol -> LString)
     ;; returns the LString for the given symbol
     ;;
     (define (expand-rule rules symbol)
       (match rules
         ['() (cons symbol '())]
         [(cons rule rest) (if (equal? (LRule-lhs rule) symbol)
                               (LRule-rhs rule)
                               (expand-rule rest symbol))]))}
    (expand-rule (LSystem-rules system) symbol)))
(check-expect (lsys-expand-symbol (LSystem '()
                                           (cons (LRule 'red
                                                        (cons 'blue '())) '()))
                                  'red) '(blue))
(check-expect (lsys-expand-symbol (LSystem '() (cons (LRule 'red
                                                            (cons 'blue '())) '()))
                                  'black) '(black))
(check-expect (lsys-expand-symbol (LSystem '() (cons (LRule 'red
                                                            (cons 'blue
                                                                  (cons 'black '()))) '()))
                                  'red) '(blue black))

(: lsys-expand-string : LSystem LString -> LString)
;; Given an L-System and a string, expand the string's symbols by the
;; rules of the system
(define (lsys-expand-string system symbols)
  (match symbols
    ['() '()]
    [(cons symbol rest) (if (Turn? symbol)
                            (cons symbol (lsys-expand-string system rest))
                            (append (lsys-expand-symbol system symbol)
                                    (lsys-expand-string system rest)))]))
(check-expect (lsys-expand-string (LSystem '()
                                           (cons (LRule 'red (cons 'blue '()))
                                                 (cons (LRule 'black (cons 'pink '())) '())))
                                  '(red black yellow))
              '(blue pink yellow))
(check-expect (lsys-expand-string (LSystem '()
                                           (cons (LRule 'yellow (cons 'blue '()))
                                                 (cons (LRule 'blue (cons 'pink '())) '())))
                                  (list 'red (Turn 60) 'black 'yellow))
              (list 'red (Turn 60) 'black 'blue))
(check-expect (lsys-expand-string (LSystem '() '()) '(red black yellow))
              '(red black yellow))

(: lsys-expand-to-depth : LSystem Integer -> LString)
;; repeatedly expand the L-system starting from its initial string
;; to the specified depth.  If the depth is <= 0, then return the
;; L-system's initial string
(define (lsys-expand-to-depth system i)
  (local
    {(define symbols : LString (LSystem-initial system))}
  (if (<= i 0)
      symbols
      (lsys-expand-to-depth (LSystem (lsys-expand-string system symbols)
                                     (LSystem-rules system)) (- i 1)))))
;; making an LSystem to test functions
;;
(define dragon-curve : LSystem (LSystem (list 'F 'X)
                                        (list (LRule 'X (list 'X (Turn 90) 'Y 'F (Turn 90)))
                                              (LRule 'Y (list (Turn -90) 'F 'X (Turn -90) 'Y)))))
(check-expect (lsys-expand-to-depth dragon-curve 2)
              (list 'F 'X (Turn 90) 'Y 'F (Turn 90) (Turn 90) (Turn -90) 'F 'X (Turn -90) 'Y 'F (Turn 90)))

;; the saved turtle state consists of its position and direction
(define-struct Turtle-State
  ([pos : Point]
   [dir : Direction]))

;; a stack of saved turtle states
(define-type State-Stack (Listof Turtle-State))

(: save-state : Turtle State-Stack -> State-Stack)
;; save the turtle's position and direction by pushing it on
;; the stack
(define (save-state turtle stk)
  (cons (Turtle-State (get-pos turtle) (get-dir turtle)) stk))

(: restore-state : Turtle Turtle-State -> Turtle)
;; restore the position and direction of a turtle from a saves state
(define (restore-state turtle state)
  (match state
    [(Turtle-State pos dir) (move-to pos (set-dir dir turtle))]))

(: lsys-draw : LString Integer (U Image-Color Pen) -> Image)
;; takes a string, an integer distance, and a color/pen and runs a turtle
;; over the commands in the string to produce an image
;;
(define (lsys-draw string length pen)
  (local
    {(define turtle : Turtle (make-turtle pen))
     (: draw-recursion : Turtle LString Turtle-State State-Stack -> Image)
     ;; takes the turtle, the L-system string that is being interpreted,
     ;; the current state of the turtle, and the stack
     ;; of saved turtle states as arguments, and which then
     ;; executes the next command in the string
     ;;
     (define (draw-recursion turtle symbols state stack)
       (match symbols
         ['() (get-image turtle)]
         [(cons (Turn theta) rest) (draw-recursion (turn theta turtle)
                                                   rest
                                                   (Turtle-State (get-pos (turn theta turtle)) (get-dir (turn theta turtle)))
                                                   stack)]
         [(cons 'F rest) (draw-recursion (forward length turtle)
                                         rest
                                         (Turtle-State (get-pos (forward length turtle)) (get-dir (forward length turtle)))
                                         stack)]
         [(cons 'f rest) (draw-recursion (forward length turtle)
                                         rest
                                         (Turtle-State (get-pos (forward length turtle)) (get-dir (forward length turtle)))
                                         stack)]
         [(cons '<< rest) (draw-recursion turtle rest state (save-state turtle stack))]
         [(cons '>> rest) (draw-recursion (restore-state turtle (first stack))
                                          rest
                                          state (match stack [(cons _ rest) rest]))]
         [(cons _ rest) (draw-recursion turtle rest state stack)]))}
    (draw-recursion turtle string (Turtle-State (Point 0 0) 0) '())))

(: lsys-render : LSystem Integer Integer (U Image-Color Pen) -> Image)
;; takes the L-system to evaluate, the depth of expansion, the distance to move forward when drawing,
;; and the pen/color to draw with
;;
(define (lsys-render system expansion length pen)
  (lsys-draw (lsys-expand-to-depth system expansion) length pen))
"Testing lsys-render/lsys-draw: creates the moore-curve"
(define moore-curve
  (local
    {(define + (Turn -90))
     (define - (Turn 90))}
    (LSystem
     (list 'L 'F 'L + 'F + 'L 'F 'L)
     (list
      (LRule 'L (list - 'R 'F + 'L 'F 'L + 'F 'R -))
      (LRule 'R (list + 'L 'F - 'R 'F 'R - 'F 'L +))))))
(lsys-render moore-curve 4 10 "red")

"Testing lsys-render/lsys-draw: creates the pythagoras tree"
(define pythagoras-tree
  (local
    {(define + (Turn -45))
     (define - (Turn 45))}
    (LSystem
     (list 'f)
     (list
      (LRule 'f (list 'F '<< + 'f '>> - 'f))
      (LRule 'F (list 'F 'F))))))
(lsys-render pythagoras-tree 6 10 "darkgreen")

"Testing lsys-render/lsys-draw: creates the koch-curve"
(define koch-curve
  (local
    {(define + (Turn -90))
     (define - (Turn 90))}
    (LSystem
     (list 'F)
     (list
      (LRule 'F (list 'F + 'F - 'F - 'F + 'F))))))
(lsys-render koch-curve 4 10 "blue")

;; run tests
;;
(test)