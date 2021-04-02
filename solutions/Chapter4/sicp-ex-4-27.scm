#lang sicp

(define count 0)

; count=0

(define (id x)
  (set! count (+ count 1))
  x)

; id=(lambda (x) ...)

(define w (id (id 10)))

; w=(id (id 10))
; (eval '(id (id 10)) env)
; (apply ('procedure ...) '(id 10) env)
; (eval-sequence (begin (set! count (+ count 1)) x) (extend ...))
; count=1
; w=('thunk '(id 10) env1)

count
; 1

w
; (force-it ('thunk ...))
; (id 10)
; count=2

count
;2
