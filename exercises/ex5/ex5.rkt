#lang typed/racket

;; CMSC15100 Winter 2017
;; Homework 5
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; Problem #1
;;

(define-struct (RTree A)
  ([value : A]                    ;; the value at a node
   [kids : (Listof (RTree A))]))  ;; the children of the node

;; defining sample RTree for testing functions
;;
(define myTree : (RTree Integer) (RTree 1 (list (RTree 2 (list (RTree 4 '()))) (RTree 3 '()))))

(: rose-map : (All (A B) (A -> B) (RTree A) -> (RTree B)))
;; maps a function over a Rose Tree
;;
(define (rose-map f tree)
  (match tree
    [(RTree a '()) (RTree (f a) '())]
    [(RTree a children) (RTree (f a) (map (lambda ([x : (RTree A)]) (rose-map f x)) children))]))

(check-expect (rose-map (lambda ([a : Integer]) 2) myTree)
              (RTree 2 (list (RTree 2 (list (RTree 2 '()))) (RTree 2 '()))))
(check-expect (rose-map (lambda ([a : Integer]) (* a a)) myTree)
              (RTree 1 (list (RTree 4 (list (RTree 16 '()))) (RTree 9 '()))))
(check-expect (rose-map (lambda ([a : Integer]) (- a a)) myTree)
              (RTree 0 (list (RTree 0 (list (RTree 0 '()))) (RTree 0 '()))))

(: rose-pre-foldl : (All (A B) (A B -> B) B (RTree A) -> B))
;; does a left-to-right pre-order reduction of the tree
;;
(define (rose-pre-foldl f initial tree)
  (local
    {(: helper : (Listof (RTree A)) B -> B)
     ;; traverses the children and creates a new tree with B values
     ;;
     (define (helper children accum)
       (match children
         ['() accum]
         [(cons x rest) (helper rest (f (RTree-value x) (helper (RTree-kids x) accum)))]))}
    (match tree
         [(RTree a '()) (f a initial)]
         [(RTree a children) (helper children (f a initial))])))

;; defining sample RTree for testing functions
;;
(define myTree2 : (RTree String) (RTree "1"
                                         (list (RTree "2" '())
                                               (RTree "3" '()))))

(check-expect (rose-pre-foldl (lambda ([a : String] [b : String]) (string-append a b)) "" myTree2)
              "321")
(check-expect (rose-pre-foldl (lambda ([a : Integer] [b : Integer]) (+ b 1)) 0 myTree)
              4)
(check-expect (rose-pre-foldl (lambda ([a : String] [b : Integer]) (+ b 1)) 0 myTree2)
              3)

(: rose-andmap : (All (A) (A -> Boolean) (RTree A) -> Boolean))
;; implements an andmap-like function for Rose Trees 
;;
(define (rose-andmap f tree)
  (match tree
    [(RTree a '()) (f a)]
    [(RTree a children) (andmap (lambda ([x : (RTree A)]) (rose-andmap f x)) children)]))

(check-expect (rose-andmap string? myTree2) #t)
(check-expect (rose-andmap (lambda ([a : Integer]) (equal? a 5)) myTree) #f)
(check-expect (rose-andmap integer? myTree) #t)

(: rose-height : (All (A) (RTree A) -> Integer))
;;  computes the height of a rose tree
;;
(define (rose-height tree)
  (match tree
    [(RTree a '()) 1]
    [(RTree a children)
     (max (foldr (lambda ([x : (RTree A)] [i : Integer]) (+ (rose-height x) 1))
                 0
                 children))]))

(check-expect (rose-height myTree2) 2)
(check-expect (rose-height myTree) 3)
(check-expect (rose-height (RTree 0 (list myTree))) 4)

;; Problem #2
;;

;; A polynomial is represented as a list of coefficients, where the
;; value at position i is the coefficient for x^i.  Furthermore,
;; polynomials have the invariant that the last element of the list is
;; guaranteed to be non-zero.
(define-type Polynomial (Listof Exact-Rational))

;; polymonials for testing functions
;;
(define poly1 : Polynomial (list 1 2 3 4 5 6 0 0 3))
(define poly2 : Polynomial (list 1 2 3 4 5 6 0 0 -3))
(define poly3 : Polynomial (list 1 2 -3 4 5 6 0 1 2))
(define poly4 : Polynomial (list 1 3 5 6 0 1 -2))

(: poly-negate : Polynomial -> Polynomial)
;; negates the coefficients of a polynomial
;;
(define (poly-negate f)
  (map (lambda ([a : Exact-Rational]) (* -1 a)) f))

(: poly-add : Polynomial Polynomial -> Polynomial)
;; adds two polynomials
;; 
(define (poly-add pol1 pol2)
  (local
    {(: add-values : Polynomial Polynomial -> Polynomial)
     ;; adds the polinomials recursively
     ;;
     (define (add-values f g)
       (match f
         ['() (if (empty? g) '() g)]
         [(cons x '()) (match g
                         ['() (cons x '())]
                         [(cons y '()) (cons (+ x y) '())]
                         [(cons y others) (cons (+ x y) others)])]
         [(cons x rest) (match g
                          ['() f]
                          [(cons y '()) (cons (+ x y) rest)]
                          [(cons y others) (cons (+ x y) (poly-add rest others))])]))
     (: truncate-zeros : Polynomial -> Polynomial)
     ;; checks and eliminates any zeros at the end of a polynomial
     (define (truncate-zeros result)
       (match result
         [(cons 0 rest) (truncate-zeros rest)]
         [(cons _ rest) (reverse result)]
         ['() '()]))}
    (truncate-zeros (reverse (add-values pol1 pol2)))))

(check-expect (poly-add poly1 poly2) '(2 4 6 8 10 12))
(check-expect (poly-add poly1 poly3) '(2 4 0 8 10 12 0 1 5))
(check-expect (poly-add poly3 poly4) '(2 5 2 10 5 7 -2 1 2))

(: poly-eval : Polynomial -> (Real -> Real))
;; returns a Real -> Real function that evaluates the polynomial at a given location
;;
(define (poly-eval poly)
  (local
    {(: evaluate : Real -> Real)
     ;; evaluates a polynomial at a given location
     ;;
     (define (evaluate x)
       (foldr (lambda ([a : Exact-Rational] [i : Real]) (+ a (* x i) )) 0 poly))}
    evaluate))

(check-expect ((poly-eval (list -2 -5 3)) 5) 48)
(check-expect ((poly-eval (poly-add poly1 poly2)) -2) -270)
(check-within ((poly-eval (list -2 4 -6 8)) 1.5) 17.5 .0001)

(: poly-derivative : Polynomial -> Polynomial)
;; computes the derivative of a polynomial
;;
(define (poly-derivative poly)
  (local
    {(: find-der : Polynomial Integer -> Polynomial)
     ;; recursively finds derivative
     ;;
     (define (find-der pol pos)
       (match pol
         ['() '()]
         [(cons x rest) (cons (* x pos) (find-der rest (+ pos 1)))]))}
    (find-der (rest poly) 1)))

(check-expect (poly-derivative poly1) '(2 6 12 20 30 0 0 24))
(check-expect (poly-derivative poly2) '(2 6 12 20 30 0 0 -24))
(check-expect (poly-derivative poly3) '(2 -6 12 20 30 0 7 16))
(check-expect (poly-derivative poly4) '(3 10 18 0 5 -12))

(: poly->string : Polynomial -> String)
;; returns a string representation of a polynomial
;;
(define (poly->string poly)
  (local
    {(: make-string : Polynomial String Integer -> String)
     ;; recursively appends values
     ;;
     (define (make-string pol accum pos)
       (match pol
         ['() accum]
         [(cons 1 '()) (string-append "x^" (number->string pos) " " accum)]
         [(cons -1 '()) (string-append "-x^" (number->string pos) " " accum)]
         [(cons x '()) (if (< x 0)
                           (string-append (number->string x) "x^" (number->string pos) " " accum)
                           (string-append (number->string x) "x^" (number->string pos) " " accum))]
         [(cons 0 rest) (make-string rest accum (+ pos 1))]
         [(cons 1 rest) (make-string rest
                                     (if (= 1 pos)
                                         (string-append "+ x " accum)
                                         (string-append "+ x^" (number->string pos) " " accum))
                                     (+ pos 1))]
         [(cons -1 rest) (make-string rest
                                     (if (= 1 pos)
                                         (string-append "- x " accum)
                                         (string-append "- x^" (number->string pos) " " accum))
                                     (+ pos 1))]
         [(cons x rest) (make-string rest
                                     (if (= 1 pos)
                                         (if (< x 0)
                                             (string-append "- " (number->string (* -1 x)) "x " accum)
                                             (string-append "+ " (number->string x) "x " accum))
                                         (if (< x 0)
                                             (string-append "- " (number->string (* -1 x))
                                                            "x^" (number->string pos) " " accum)
                                             (string-append "+ "(number->string x) "x^"
                                                            (number->string pos) " " accum)))
                                     (+ pos 1))]))}
    (make-string (rest poly) (if (< (first poly) 0)
                                 (string-append "- " (number->string (* (first poly) -1)))
                                 (string-append "+ " (number->string (first poly)))) 1)))

(check-expect (poly->string (list -1 2 3 4 5 6 0 0 1)) "x^8 + 6x^5 + 5x^4 + 4x^3 + 3x^2 + 2x - 1")
(check-expect (poly->string (list 1 2 3 1 5 6 0 0 -3)) "-3x^8 + 6x^5 + 5x^4 + x^3 + 3x^2 + 2x + 1")
(check-expect (poly->string (list 1 2 -3 4 5 6 0 -1 2)) "2x^8 - x^7 + 6x^5 + 5x^4 + 4x^3 - 3x^2 + 2x + 1")
(check-expect (poly->string (list 1 3 5 6 0 1 -2)) "-2x^6 + x^5 + 6x^3 + 5x^2 + 3x + 1")

;; Probelm 3
;;

;; Variables are represented as strings
;;
(define-type Identifier String)

;; Integer expression are either arithmetic expressions (IArith),
;; Literal integers (ILit), local definitions (ILocal), or
;; variables (IVar)
;;
(define-type IExp (U IArith ILit ILocal IVar))

;; An arithmetic expression consists of an operator and
;; one or more argument expressions
;;
(define-struct IArith
  ([op : (U '+ '* '-)]      ;; the operator (represented as a symbol)
   [arg : IExp]             ;; the first argument
   [args : (Listof IExp)])) ;; additional arguments

;; An integer literal
;;
(define-struct ILit ([val : Integer]))

;; A local definition
;;
(define-struct ILocal
  ([id  : Identifier]  ;; the variable being defined
   [val : IExp]        ;; the value of the variable
   [in  : IExp]))      ;; the scope of the variable

;; A variable occurrence
;;
(define-struct IVar ([id : Identifier]))

(: exp->string : IExp -> String)
;; maps the IExp type to strings in Racket-style prefix notation
;;
(define (exp->string expression)
  (match expression
    [(IArith op arg args) (string-append (match op ['* "(*"] ['+ "(+"] ['- "(-"])
                                         (exp->string arg)
                                         (foldr (lambda ([x : IExp] [y : String])
                                                  (string-append (exp->string x) y)) "" args)
                                         ")")]
    [(ILit val) (string-append " " (number->string val))]
    [(ILocal id val in) (string-append "(local {(define " id " "
                                       (exp->string val) ")} " (exp->string in) ")")]
    [(IVar id) (string-append " " id)]))

(check-expect (exp->string
               (ILocal "x" (IArith '+ (ILit 2) (list (ILit 5)))
                       (ILocal "y" (IArith '* (ILit 3) (list (ILit 2)))
                               (IArith '* (IVar "y") (list (IVar "x"))))))
              "(local {(define x (+ 2 5))} (local {(define y (* 3 2))} (* y x)))")

;; a variable binding
;;
(define-struct Binding
  ([id : Identifier]    ;; the name of the variable
   [val : Integer]))    ;; its value

(: eval : IExp (Listof Binding) -> Integer)
;; takes an expression and a list of Binding values and evaluates the expression
;; The Binding type represents a mapping from an identifier to a value.
;;
(define (eval expression variables)
  (local
    {(: get-value : String (Listof Binding) -> Integer)
     ;; takes a string from a variable and finds the value in the binding list
     ;;
     (define (get-value var list)
       (match list
         ['() (error (string-append "eval: no variable stored with id \"" var "\""))]
         [(cons (Binding id val) rest) (if (equal? id var)
                                           val
                                           (get-value var rest))]))}
    (match expression
      [(IArith op arg args) (match op
                              ['* (local {}
                                    (* (eval arg variables)
                                       (match args
                                         ['() 1]
                                         [(cons x rest) (eval (IArith op x rest) variables)])))]
                              ['+ (local {}
                                    (+ (eval arg variables)
                                       (match args
                                         ['() 0]
                                         [(cons x rest) (eval (IArith op x rest) variables)])))]
                              ['- (local {}
                                    (- (eval arg variables)
                                       (match args
                                         ['() 0]
                                         [(cons x rest) (eval (IArith op x rest) variables)])))])]
      [(ILocal id val in) (eval in (cons (Binding id (eval val variables)) variables))]
      [(ILit val) val]
      [(IVar id) (get-value id variables)])))

(check-expect (eval (IArith '+ (IVar "x") (list (ILit 5) (IArith '- (ILit 2) (list (ILit 5)))))
                    (list (Binding "x" 2))) 4)
(check-expect (eval (IArith '+ (ILit 5) (list (ILit 5) (IArith '- (ILit 2) (list (ILit 5)))))
                    '()) 7)
(check-expect (eval
               (ILocal "x" (IArith '+ (ILit 2) (list (ILit 5)))
                       (ILocal "y" (IArith '* (ILit 3) (list (ILit 2)))
                               (IArith '* (IVar "y") (list (IVar "x"))))) '())
              42)

;; run tests
;;
(test)