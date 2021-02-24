#lang sicp

(define (make-accumulator first-n)
  (lambda (x)
    (begin
      (set! first-n (+ first-n x))
      first-n)))

(define A (make-accumulator 5))
(A 10)
; 15
(A 10)
; 25
