#lang sicp
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; normal-order evaluation: 0 since we don't need to call (p)
; (test 0 (p))
; (if (= 0 0) 0 y))
; (if #t) 0 y))
; 0

; applicative-order evaluation: never give me a result 
; Because calling (p) always enter an infinite loop
; (test 0 (p))
; (test 0 (p))
; (test 0 (p))
; ...
