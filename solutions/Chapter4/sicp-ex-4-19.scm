#lang sicp

; In scheme, we use Alyssa' method

(let ((a 1))
  (define (f x)
    (define (b) (+ (a) x))
    (define (a) 5)
    (+ (a) (b)))
  (f 10))
; 20

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; a: undefined;
; cannot use before initialization

; It's hard to implement eva's idea
; we should sort the definitions and define a first, then b
; since in the definition of b, we use the value of a
; That's to say, we need to do some topological sorting
; but in some case, we cannot do this
