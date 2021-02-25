#lang sicp

(define (f)
  (let ((constant 1))
    (lambda (x)
      (set! constant (* constant x))
      constant)))

(define f1 (f))
(define f2 (f))

(+ (f1 0) (f1 1))

; left to right
; constant=1 (+ (f 0) (f 1))
; constant=0 (+ 0 (f 1))
; constant=0 (+ 0 0)
; 0

(+ (f2 1) (f2 0))

; right to left
; constant=1 (+ (f 0) (f 1))
; constant=1 (+ (f 0) 1)
; constant=0 (+ 0 1)
; 1
