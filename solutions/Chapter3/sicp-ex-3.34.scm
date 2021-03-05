#lang sicp

(define (squarer a b)
  (multiplier a a b))

; if b=?, a is set, we get b=a*a
; if a=?, b is set, we get a=?
; since in mutiplier, we need at least two values to compute the third one.
