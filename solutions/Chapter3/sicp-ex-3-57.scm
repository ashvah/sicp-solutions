#lang sicp

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(stream-ref fibs n)
; (0 (1 (add-streams (stream-cdr fibs) fibs)))
; (1 (add-streams (stream-cdr fibs) fibs))
; (add-streams (stream-cdr fibs) fibs)
; (stream-map + (stream-cdr fibs) fibs)
; (0+1 (stream-map ...))  1 -> 1
; (1+1 (stream-map ...))  2 -> 1+1+1
;
; (fib(n) (stream-map ...))  n-1 -> T(n-1)+1+T(n-1)=O(2^n)
