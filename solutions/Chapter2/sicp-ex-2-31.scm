#lang sicp

(define (square x)
  (* x x))

(define (tree-map1 f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map1 f (car tree))
                    (tree-map1 f (cdr tree))))))

(define (tree-map2 f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map2 f subtree)
             (f subtree)))
       tree))

(list 1 (list 2 (list 3 4) 5) (list 6 7))
; (1 (2 (3 4) 5) (6 7))
(tree-map1 square (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
(tree-map2 square (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
