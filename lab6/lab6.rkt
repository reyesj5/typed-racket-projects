#lang typed/racket

;; CMSC15100 Winter 2017
;; Labratory 6
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; An (Option T) is either 'None or (Some x), where x has type T
(define-type (Option T) (U 'None (Some T)))
(define-struct (Some T) ([value : T]))

;; A (Grid nr nc chrs) represents a rectangular grid of characters, where
;; nr is the number of rows, nc is the number of columns, and chrs is a
;; list of (nr*nc) characters that comprise the grid.
(define-struct Grid
  ([num-rows : Integer]
   [num-cols : Integer]
   [content : (Listof Char)]))

(: make-grid : Integer Integer (Listof Char) -> Grid)
;; takes the number of rows, the number of columns, and the contents of the grid,
;; checks the validity of the arguments, and returns a Grid with the given properties.
;;
(define (make-grid r c contents)
  (if (and (not (empty? contents)) (= (* r c) (length contents)))
      (Grid r c contents)
      (error "make-grid: invalid grid")))
(check-error (make-grid 2 5 '()) "make-grid: invalid grid")
(check-error (make-grid 2 5 (list #\A #\D #\E #\P #\Z)) "make-grid: invalid grid")
(check-expect (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                   #\E #\O #\P #\A #\L))
             (Grid 2 5 (list #\A #\D #\E #\P #\Z
                             #\E #\O #\P #\A #\L)))
;; Grid to tests functions
;;
(define example-grid : Grid
  (make-grid
    4 5 (list #\A #\D #\E #\P #\Z
              #\E #\O #\P #\A #\L
              #\K #\Z #\I #\F #\W
              #\F #\T #\H #\I #\N)))

;; a pair of integers, where r specifies a row in a grid and c specifies a column
;; representing the reference of a character in a grid
;;
(define-struct Cell
  ([r : Integer]
   [c : Integer]))

(: grid-ref : Grid Cell -> Char)
;;  returns the character at the specified cell of the grid.
;;
(define (grid-ref grid cell)
  (list-ref (Grid-content grid) (+ (* (Grid-num-cols grid) (Cell-r cell)) (Cell-c cell))))
(check-expect (grid-ref (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                             #\E #\O #\P #\A #\L))
                        (Cell 1 1)) #\O)
(check-expect (grid-ref (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                             #\E #\O #\P #\A #\L))
                        (Cell 0 4)) #\Z)
(check-expect (grid-ref (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                             #\E #\O #\P #\A #\L))
                        (Cell 0 0)) #\A)

(: cell=? : Cell Cell -> Boolean)
;; Tests if two cells are equal; checks if characters are equal
;;
(define (cell=? cell1 cell2)
  (equal? cell1 cell2))
(check-expect (cell=? (Cell 2 5) (Cell 2 5)) #t)
(check-expect (cell=? (Cell 7 3) (Cell 7 3)) #t)
(check-expect (cell=? (Cell 1 0) (Cell 0 0)) #f)

;; A Path specifies a sequence of cells in the grid that make up a word
;;
(define-type Path (Listof Cell)) 

(: valid-cell? : Grid -> Cell -> Boolean)
;; given a grid, returns a function for checking the validity of a cell in that grid
;;
(define (valid-cell? grid)
  ;; A cell is valid if its row and column are non-negative
  ;; and less that the number of rows and columns (respectively) in the grid.
  (local
    {(: valid? : Cell -> Boolean)
     ;; checking the validity of a cell in that grid
     ;;
     (define (valid? cell)
       (and (< -1 (Cell-r cell) (Grid-num-rows grid))
            (< -1 (Cell-c cell) (Grid-num-cols grid))))}
    valid?))
(check-expect ((valid-cell? (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                             #\E #\O #\P #\A #\L))) (Cell 1 2)) #t)
(check-expect ((valid-cell? (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                             #\E #\O #\P #\A #\L))) (Cell 2 2)) #f)
(check-expect ((valid-cell? (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                             #\E #\O #\P #\A #\L))) (Cell -1 2)) #f)

(: adjacent? : Cell Cell -> Boolean)
;; tests if two cells are adjacent in a grid
;;
(define (adjacent? cell1 cell2)
  (local
    {(: offsets : Cell -> (Listof Cell))
     ;; creates a list with the offsets of the first cell
     ;;
     (define (offsets cell)
       (local
         {(define r : Integer (Cell-r cell))
          (define c : Integer (Cell-c cell))}
         (list (Cell (- r 1) (- c 1)) (Cell (- r 1) c) (Cell (- r 1) (+ c 1))
               (Cell r (- c 1)) (Cell r (+ c 1))
               (Cell (+ r 1) (- c 1)) (Cell (+ r 1) c) (Cell (+ r 1) (+ c 1)))))}
    (ormap (lambda ([x : Cell]) (equal? cell2 x)) (offsets cell1))))
(check-expect (adjacent? (Cell 2 5) (Cell 2 6)) #t)
(check-expect (adjacent? (Cell 2 5) (Cell 2 5)) #f)
(check-expect (adjacent? (Cell 4 5) (Cell 2 6)) #f)
(check-expect (adjacent? (Cell 2 5) (Cell 2 4)) #t)
(check-expect (adjacent? (Cell 2 5) (Cell 3 6)) #t)
(check-expect (adjacent? (Cell 0 2) (Cell 1 1)) #t)

(: has-loop? : Path -> Boolean)
;;  tests for loops
;;
(define (has-loop? path)
  (match path
    ['() #f]
    [(cons x '()) #f]
    [(cons x rest) (if (or (ormap (lambda ([y : Cell]) (equal? y x)) rest)
                           (has-loop? rest)) #t #f)]))
(check-expect (has-loop? (list (Cell 2 3) (Cell 5 6) (Cell 1 0))) #f)
(check-expect (has-loop? (list (Cell 2 3))) #f)
(check-expect (has-loop? '()) #f)
(check-expect (has-loop? (list (Cell 2 3) (Cell 5 6) (Cell 1 0) (Cell 2 3))) #t)

(: valid-path? : Grid Path -> Boolean)
;;  tests for valid paths
;;
(define (valid-path? grid path)
  (local
    {(: helper-tests : Grid Path -> Boolean)
     ;; tests for valid cels and for adjacent cells
     ;;
     (define (helper-tests grid path)
       (match path
         ['() #f]
         [(cons x '()) #t]
         [(cons x rest) (if (and ((valid-cell? grid) x) (adjacent? x (first rest)))
                            (helper-tests grid rest)
                            #f)]))}
    (if (has-loop? path)
        #f
        (helper-tests grid path))))
(check-expect (valid-path? (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                                #\E #\O #\P #\A #\L))
                           (list (Cell 1 3) (Cell 0 2) (Cell 1 2) (Cell 1 3))) #f)
(check-expect (valid-path? (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                                #\E #\O #\P #\A #\L))
                           (list (Cell 2 3) (Cell 5 6) (Cell 1 0) (Cell 2 7))) #f)
(check-expect (valid-path? (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                                #\E #\O #\P #\A #\L))
                           (list (Cell 1 3) (Cell 0 2) (Cell 1 1) (Cell 1 0))) #t)

(: path->string : Grid Path -> (Option String))
;; returns 'None, if the path is invalid, or returns (Some w),
;; where w is the word specified by the path
;;
(define (path->string grid path)
  (local
    {(: get-chars : Grid Path String -> (Option String))
     ;; obtains the characters specified by a valid path
     (define (get-chars grid path word)
       (match path
         ['() (Some word)]
         [(cons x rest)
          (get-chars grid rest (string-append word (string (grid-ref grid x))))]))}
    (if (valid-path? grid path) (get-chars grid path "") 'None)))
(check-expect (path->string (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                                 #\E #\O #\P #\A #\L))
                            (list (Cell 1 2) (Cell 0 2) (Cell 1 3))) (Some "PEA"))
(check-expect (path->string (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                                 #\E #\O #\P #\A #\L))
                            (list (Cell 0 1) (Cell 1 1) (Cell 1 2) (Cell 0 2))) (Some "DOPE"))
(check-expect (path->string (make-grid 2 5 (list #\A #\D #\E #\P #\Z
                                                 #\E #\O #\P #\A #\L))
                            (list (Cell 1 4) (Cell 1 0) (Cell 0 0) (Cell 1 8))) 'None)

;; A dictionary is just a list of accepted words
;;
(define-type Dictionary (Listof String))

;; sample dictionary used to tests following functions
(define example-dictionary : Dictionary
  (list "ADD" "ADO" "APE" "APED" "APP" "DEAF" "DOE" "DOZE" "FAITH" "FIT" "HIP" "HIT" "HOP"
        "HOPE" "LAP" "ODE" "PAP" "PEA" "PED" "PEP" "PIT" "PITH" "POD" "POKE" "POKED" "TIP"
        "TOP" "WAIT" "ZAP" "ZED" "ZIP" "ZIT"))

(: word-in-dict? : String Dictionary -> Boolean)
;;  tests if a given word is in a dictionary
;;
(define (word-in-dict? word dict)
  (ormap (lambda ([x : String]) (equal? word x)) dict))
(check-expect (word-in-dict? "ZIT" example-dictionary) #t)
(check-expect (word-in-dict? "ODE" example-dictionary) #t)
(check-expect (word-in-dict? "CAR" example-dictionary) #f)

(: path-in-dict? : Grid Dictionary -> Path -> Boolean)
;; takes a grid and a dictionary and returns a function
;; for testing if the word spcified by a path is in the dictionary
;;
(define (path-in-dict? grid dict)
  (local
    {(: check-word : Path -> Boolean)
     ;; checks if the word in a path is in the dictionary
     ;;
     (define (check-word path)
       (local
         {(: get-value : (All(T) (Option T) T -> T))
          ;;returns the contents of an optional value or the default
          ;;
          (define(get-value opt default)
            (match opt
              [’None default]
              [(Some val) val]))
          (define word : (Option String) (path->string grid path))}
         (match word
           ['None #f]
           [(Some string) (word-in-dict? string dict)])))}
    check-word))
(check-expect ((path-in-dict? example-grid example-dictionary)
               (list (Cell 2 1) (Cell 2 2) (Cell 1 2))) #t)
(check-expect ((path-in-dict? example-grid example-dictionary)
               (list (Cell 2 1) (Cell 2 2) (Cell 1 2) (Cell 1 3))) #f)
(check-expect ((path-in-dict? example-grid example-dictionary)
               (list (Cell 2 1) (Cell 2 2) (Cell 3 1))) #t)

;; run tests
;;
(test)