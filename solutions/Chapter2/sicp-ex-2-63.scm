#lang sicp

; a) produce the same list
(define T (list 3 (list 1 nil nil) (list 5 (list 4 nil nil) (list 9 nil nil))))
T
(tree->list-1 T)
; (1 3 4 5 9)
(tree->list-2 T)
; (1 3 4 5 9)

; b)
; T(2n)=2T(n)+O(n)=>T(n)=O(nlogn)
; T(2n)=2T(n)+O(1)=>T(n)=O(n)
