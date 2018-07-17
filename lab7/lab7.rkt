#lang typed/racket

;; CMSC15100 Winter 2017
;; Labratory 7
;; <Jose Reyes>

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; load custom definitions
;;
(require "../include/uchicago151.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility types and functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A pair of values
;;
(define-struct (Pair A B)
  ([fst : A]
   [snd : B]))

;; An (Option T) is either 'None or (Some x), where x has type T
(define-type (Option T) (U 'None (Some T)))
(define-struct (Some T) ([value : T]))

;; Represents the possible relations between totally-ordered values
(define-type Order (U '< '= '>))

;; The type of a comparison function
(define-type (Cmp A) (A A -> Order))

(: integer-cmp : (Cmp Integer))
;; comparisons of integer keys
;;
(define (integer-cmp a b)
  (cond
    [(< a b) '<]
    [(= a b) '=]
    [else '>]))

(: string-cmp : (Cmp String))
;; comparisons of string keys
;;
(define (string-cmp a b)
  (cond
    [(string<? a b) '<]
    [(string=? a b) '=]
    [else '>]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Binary Trees
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A binary tree is either 'Leaf or a (Node l v r), where l and r are binary
;; trees and v is a value
(define-type (Tree A) (U 'Leaf (Node A)))

;; A node in a binary tree
(define-struct (Node A)
  ([left : (Tree A)]
   [value : A]
   [right : (Tree A)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finite Maps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A finite map from key type K to value type T is represented as the value
;; (Map cmp bst), where cmp is a comparison function that defines a total
;; ordering on keys, and bst is a binary tree annotated with key-value pairs
;; that satisfies the Binary-Search-Tree invariant.
;;
(define-struct (Map K T)
  ([cmp : (Cmp K)]
   [bst : (Tree (Pair K T))]))

(: make-empty-map : (All (K T) (Cmp K) -> (Map K T)))
;; create an empty map from the given comparison
;;
(define (make-empty-map cmp) (Map cmp 'Leaf))

(: map-find : (All (K T) (Map K T) K -> (Option T)))
;; apply a finite map to a key (i.e., lookup the key in the map).  Return (Some v)
;; if (Pair key v) is in the map, otherwise return 'None
;;
(define (map-find map key)
  (match map
    [(Map cmp bst)
     (local
       {(: find : (Tree (Pair K T)) -> (Option T))
        ;; search the BST for the key
        (define (find bst)
          (match bst
            ['Leaf 'None]
            [(Node l (Pair k v) r)
             (match (cmp key k)
               ['< (find l)]
               ['= (Some v)]
               ['> (find r)])]))}
       (find bst))]))

;; A test map
(define test-map1 : (Map Integer String)
  (Map
   integer-cmp
   (Node
    (Node
     (Node 'Leaf (Pair 0 "0") 'Leaf)
     (Pair 2 "2")
     (Node 'Leaf (Pair 3 "3") 'Leaf))
    (Pair 5 "5")
    (Node
     (Node 'Leaf (Pair 6 "6") 'Leaf)
     (Pair 8 "8")
     (Node 'Leaf (Pair 10 "10") 'Leaf)))))

(check-expect (map-find ((inst make-empty-map Integer String) integer-cmp) 1) 'None)
(check-expect (map-find test-map1 -1) 'None)
(check-expect (map-find test-map1  0) (Some "0"))
(check-expect (map-find test-map1  1) 'None)
(check-expect (map-find test-map1  2) (Some "2"))
(check-expect (map-find test-map1  3) (Some "3"))
(check-expect (map-find test-map1  4) 'None)
(check-expect (map-find test-map1  5) (Some "5"))
(check-expect (map-find test-map1  6) (Some "6"))
(check-expect (map-find test-map1  7) 'None)
(check-expect (map-find test-map1  8) (Some "8"))
(check-expect (map-find test-map1  9) 'None)
(check-expect (map-find test-map1 10) (Some "10"))
(check-expect (map-find test-map1 11) 'None)

(: map-insert : (All (K T) (Map K T) K T -> (Map K T)))
;; insert the key-value pair into the finite map.
;;
(define (map-insert map key value)
  (match map
    [(Map cmp bst)
     (local
       {(: insert : (Tree (Pair K T)) T -> (Tree (Pair K T)))
        ;; search the BST for the right position
        (define (insert bst value)
          (match bst
            ['Leaf (Node 'Leaf (Pair key value) 'Leaf)]
            [(Node left (Pair k v) right)
             (match (cmp key k)
               ['< (Node (insert left value) (Pair k v) right)]
               ['= (Node left (Pair key value) right)]
               ['> (Node left (Pair k v) (insert right value))])]))}
       (Map cmp (insert bst value)))]))

"testing map-insert: gives a tree with Pair 1 "1") added to map to the right
child with (Pair 0 "0")"
(map-insert test-map1 1 "1")
(check-expect (map-find (map-insert test-map1 7 "test") 7) (Some "test"))
(check-expect (map-find (map-insert test-map1 5 "test") 5) (Some "test"))

(: bst-remove-min : (All (A) (Tree A) -> (Pair A (Tree A))))
;; remove the minimum node from a non-empty BST.  Return a pair of the node's
;; value and the residual tree.
;;
(define (bst-remove-min bst)
  (match bst
    ['Leaf (error "bst-remove-min: expected non-empty tree")]
    [(Node 'Leaf x right) (Pair x right)]
    [(Node left x right) (local
                           {(define pair : (Pair A (Tree A)) (bst-remove-min left))}
                           (Pair (Pair-fst pair) (Node (Pair-snd pair) x right)))]))

(check-expect (bst-remove-min (match test-map1 [(Map cmp bst) bst]))
              (Pair (Pair 0 "0") (Node
                                  (Node
                                   'Leaf
                                   (Pair 2 "2")
                                   (Node 'Leaf (Pair 3 "3") 'Leaf))
                                  (Pair 5 "5")
                                  (Node
                                   (Node 'Leaf (Pair 6 "6") 'Leaf)
                                   (Pair 8 "8")
                                   (Node 'Leaf (Pair 10 "10") 'Leaf)))))
(check-expect (bst-remove-min (match (map-insert test-map1 -1 "test")
                                [(Map cmp bst) bst]))
              (Pair (Pair -1 "test") (Node
                                      (Node
                                       (Node 'Leaf (Pair 0 "0") 'Leaf)
                                       (Pair 2 "2")
                                       (Node 'Leaf (Pair 3 "3") 'Leaf))
                                      (Pair 5 "5")
                                      (Node
                                       (Node 'Leaf (Pair 6 "6") 'Leaf)
                                       (Pair 8 "8")
                                       (Node 'Leaf (Pair 10 "10") 'Leaf)))))
(check-expect (bst-remove-min (match (map-insert test-map1 -5 "test")
                                [(Map cmp bst) bst]))
              (Pair (Pair -5 "test") (Node
                                      (Node
                                       (Node 'Leaf (Pair 0 "0") 'Leaf)
                                       (Pair 2 "2")
                                       (Node 'Leaf (Pair 3 "3") 'Leaf))
                                      (Pair 5 "5")
                                      (Node
                                       (Node 'Leaf (Pair 6 "6") 'Leaf)
                                       (Pair 8 "8")
                                       (Node 'Leaf (Pair 10 "10") 'Leaf)))))

(: bst-remove-root : (All (A) (Tree A) -> (Tree A)))
;; remove the root from the tree and restore the BST structure.  Return 'Leaf
;; for the empty tree.
;;
(define (bst-remove-root bst)
  (match bst
    ['Leaf 'Leaf]
    [(Node left x 'Leaf) left]
    [(Node left x right) (local
                               {(define min-pair : (Pair A (Tree A)) (bst-remove-min right))}
                               (Node left (Pair-fst min-pair) (Pair-snd min-pair)))]))

(check-expect (bst-remove-root 'Leaf) 'Leaf)
(check-expect (bst-remove-root (Node 'Leaf 5 'Leaf)) 'Leaf)
(check-expect (bst-remove-root (match test-map1 [(Map cmp bst) bst]))
              (Node
               (Node
                (Node 'Leaf (Pair 0 "0") 'Leaf)
                (Pair 2 "2")
                (Node 'Leaf (Pair 3 "3") 'Leaf))
               (Pair 6 "6")
               (Node
                'Leaf
                (Pair 8 "8")
                (Node 'Leaf (Pair 10 "10") 'Leaf))))

(: map-remove : (All (K T) (Map K T) K -> (Map K T)))
;; remove the key from the finite map.  If the key is not present, then this
;; operation has no effect on the map.
;;
(define (map-remove map key)
  (match map
    [(Map cmp bst)
     (local
       {(: remove : (Tree (Pair K T)) K -> (Tree (Pair K T)))
        ;; search the BST for the right position and removes node
        (define (remove bst key)
          (match bst
            ['Leaf 'Leaf]
            [(Node left (Pair k v) right)
             (match (cmp key k)
               ['< (Node (remove left key) (Pair k v) right)]
               ['= (bst-remove-root (Node left (Pair k v) right))]
               ['> (Node left (Pair k v) (remove right key))])]))}
       (Map cmp (remove bst key)))]))

(check-expect (Map-bst (map-remove test-map1 8))
              (Node
               (Node
                (Node 'Leaf (Pair 0 "0") 'Leaf)
                (Pair 2 "2")
                (Node 'Leaf (Pair 3 "3") 'Leaf))
               (Pair 5 "5")
               (Node
                (Node 'Leaf (Pair 6 "6") 'Leaf)
                (Pair 10 "10") 'Leaf)))
(check-expect (Map-bst (map-remove test-map1 0))
              (Node
               (Node
                'Leaf
                (Pair 2 "2")
                (Node 'Leaf (Pair 3 "3") 'Leaf))
               (Pair 5 "5")
               (Node
                (Node 'Leaf (Pair 6 "6") 'Leaf)
                (Pair 8 "8")
                (Node 'Leaf (Pair 10 "10") 'Leaf))))
(check-expect (Map-bst (map-remove test-map1 10))
              (Node
               (Node
                (Node 'Leaf (Pair 0 "0") 'Leaf)
                (Pair 2 "2")
                (Node 'Leaf (Pair 3 "3") 'Leaf))
               (Pair 5 "5")
               (Node
                (Node 'Leaf (Pair 6 "6") 'Leaf)
                (Pair 8 "8")
                'Leaf)))

;; run tests
;;
(test)