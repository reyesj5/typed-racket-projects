#lang typed/racket

;; CMSC15100 Winter 2017
;; Homework 6
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;; Problem 1
;;

;; sample vector to test functions
;;
(define sample-vec : (Vectorof Integer) (vector 1 2 3 4 5 4 3 2 1))

;; sample vector to test functions
;;
(define sample-vec2 : (Vectorof Integer) (vector 1 2 3 4 5 5 4 3 2 1))

;; sample vector to test functions
;;
(define sample-vec3 : (Vectorof Integer) (vector 1 2 3 4 5 1 4 3 2 1))

(: palindrome? : (Vectorof Integer) -> Boolean)
;; returns #t if its argument is a palindrome
;;
(define (palindrome? vector)
  (local
    {(define v-length : Integer (vector-length vector))
     (: compare : (Vectorof Integer) Integer Integer -> Boolean)
     ;; compares each element of the vector with its mirror on the other half of the vector
     ;;
     (define (compare vec i j)
       (if (= i v-length)
           #t
           (if (equal? (vector-ref vec i) (vector-ref vec j))
               (compare vec (+ i 1) (- j 1))
               #f)))}
    (compare vector 0 (- v-length 1))))

(check-expect (palindrome? sample-vec) #t)
(check-expect (palindrome? sample-vec2) #t)
(check-expect (palindrome? sample-vec3) #f)

;; Problem 2
;;

;; An (Option T) is either 'None or (Some v), where v has type T
(define-type (Option T) (U 'None (Some T)))
(define-struct (Some T) ([value : T]))

;; A recursive representation of binary trees
(define-type (Tree A) (U 'Empty (Node A)))
(define-struct (Node A)
  ([left  : (Tree A)]   ;; left child
   [val   : A]          ;; value at this node
   [right : (Tree A)])) ;; right child

;; a vector representation of a complete binary tree
(define-type (VTree A) (Vectorof A))

(: in-tree? : (All (A) (VTree A) Integer -> Boolean))
;; returns true if the given index specifies a node in the tree.
;;
(define (in-tree? vec i)
  (if (< -1 i (vector-length vec)) #t #f))

(check-expect (in-tree? sample-vec 9) #f)
(check-expect (in-tree? sample-vec -1) #f)
(check-expect (in-tree? sample-vec 5) #t)

(: left-child : (All (A) (VTree A) Integer -> (Option Integer)))
;; returns the index of the specified node’s left child,
;; or 'None if there is no left child.
(define (left-child vec i)
  (local
    {(define left-index : Integer (+ (* 2 i) 1))}
    (if (in-tree? vec left-index) (Some left-index) 'None)))

(: right-child : (All (A) (VTree A) Integer -> (Option Integer)))
;; returns the index of the specified node’s right child,
;; or 'None if there is no right child.
;;
(define (right-child vec i)
  (local
    {(define right-index : Integer (+ (* 2 i) 2))}
    (if (in-tree? vec right-index) (Some right-index) 'None)))

(check-expect (right-child sample-vec 3) (Some 8))
(check-expect (right-child sample-vec 0) (Some 2))
(check-expect (right-child sample-vec 4) 'None)

(: parent : (All (A) (VTree A) Integer -> (Option Integer)))
;; returns the index of the specified node’s parent,
;; or 'None if the node is the root.
(define (parent vec i)
  (if (in-tree? vec i)
      (if (= i 0)
          'None
          (match (remainder i 2)
            [0 (Some (quotient (- i 2) 2))]
            [1 (Some (quotient (- i 1) 2))]))
      'None))

(check-expect (parent sample-vec 5) (Some 2))
(check-expect (parent sample-vec 0) 'None)
(check-expect (parent sample-vec 9) 'None)

(: values-of : (All (A) (VTree A) -> (Listof A)))
;; returns the list of values in the tree corresponding to a
;; left-to-right pre-order traversal of the tree.
;;
(define (values-of vec)
  (local
    {(: visited? : Integer (Listof Integer) -> Boolean)
     ;; checks if given index has been visited before
     ;;
     (define (visited? i list)
       (match list
         ['() #f]
         [(cons x rest) (if (= i x) #t (visited? i rest))]))
     (: add-value : Integer (Listof Integer) -> (Listof A))
     ;; adds value at given index to the list, with index of -1 signaling there are
     ;; no more non-visited nodes in the tree
     ;;
     (define (add-value i visited)
       (if (in-tree? vec i)
           (match (visited? i visited)
             [#f (cons (vector-ref vec i) (add-value (match (left-child vec i)
                                                       ['None (match (right-child vec i)
                                                                ['None (match (parent vec i)
                                                                         ['None -1]
                                                                         [(Some z) z])]
                                                                [(Some y) y])]
                                                       [(Some x) x])
                                                     (cons i visited)))]
             [#t (add-value (match (right-child vec i)
                              ['None (match (parent vec i)
                                       ['None -1]
                                       [(Some x) x])]
                              [(Some x) (if (visited? x visited)
                                            (match (parent vec i)
                                              ['None -1]
                                              [(Some y) y])
                                            x)])
                            (cons i visited))])
           '()))}
    (add-value 0 '())))

(check-expect (values-of sample-vec) '(1 2 4 2 1 5 3 4 3))
(check-expect (values-of (vector )) '())
(check-expect (values-of (vector 'A 'B 'E 'C 'D 'F)) (list 'A 'B 'C 'D 'E 'F))

(: vtree->tree : (All (A) (VTree A) -> (Tree A)))
;; returns the recursive-tree representation corresponding to the given vector tree.
(define (vtree->tree vec)
  (local
     {(: make-tree : Integer -> (Tree A))
      ;; adds value at given index to the list
      ;;
      (define (make-tree i)
        (Node (match (left-child vec i)
                ['None 'Empty]
                [(Some x) (make-tree x)])
              (vector-ref vec i)
              (match (right-child vec i)
                ['None 'Empty]
                [(Some x) (make-tree x)])))}
    (if (= (vector-length vec) 0) 'Empty (make-tree 0))))

(check-expect (vtree->tree sample-vec)
              (Node
               (Node
                (Node
                 (Node 'Empty 2 'Empty)
                 4
                 (Node 'Empty 1 'Empty))
                2
                (Node 'Empty 5 'Empty))
               1
               (Node
                (Node 'Empty 4 'Empty)
                3
                (Node 'Empty 3 'Empty))))
(check-expect (vtree->tree (vector )) 'Empty)
(check-expect (vtree->tree sample-vec3)
              (Node
               (Node
                (Node
                 (Node 'Empty 3 'Empty)
                 4
                 (Node 'Empty 2 'Empty))
                2
                (Node (Node 'Empty 1 'Empty) 5 'Empty))
               1
               (Node
                (Node 'Empty 1 'Empty)
                3
                (Node 'Empty 4 'Empty))))

;; Problem 3
;;

;; Graph nodes are identified by integer indices and a graph is represented
;; by a vector of adjacency lists.
(define-type Graph (Vectorof (Listof Integer)))

;; sample graph for testing functions
;;
(define sample-graph : Graph (vector '(1 3) '(0 2) '(0) '(1 0)))

(: valid-edge? : Graph -> (Integer Integer -> Boolean))
;; ((valid-edge? g) u v) returns true if u->v is in the graph g.
;;
(define (valid-edge? graph)
  (local
    {(: test-nodes : Integer Integer -> Boolean)
     ;; takes two integers and tests for a valid path
     ;;
     (define (test-nodes x y)
       (if (= x y)
           #t
           (ormap (lambda ([i : Integer]) (= i y)) (vector-ref graph x))))}
    test-nodes))

(check-expect ((valid-edge? sample-graph) 1 0) #t)
(check-expect ((valid-edge? sample-graph) 1 1) #t)
(check-expect ((valid-edge? sample-graph) 3 2) #f)
(check-expect ((valid-edge? sample-graph) 0 3) #t)

(: valid-path? : Graph (Listof Integer) -> Boolean)
;; (valid-path? g path) returns true if the path exists in the graph g. 
;;
(define (valid-path? graph path)
  (match path
    ['() #f]
    [(list x) #t]
    [(cons x rest) (if ((valid-edge? graph) x (first rest))
                       (valid-path? graph rest)
                       #f)]))

(check-expect (valid-path? sample-graph '()) #f)
(check-expect (valid-path? sample-graph '(2)) #t)
(check-expect (valid-path? sample-graph '(2 0 3 1 2 0 1 0)) #t)
(check-expect (valid-path? sample-graph '(2 0 3 1 2 3 1 0)) #f)

;; run tests
;;
(test)