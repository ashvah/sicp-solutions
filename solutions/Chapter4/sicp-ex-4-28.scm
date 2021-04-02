#lang sicp

(define f (cons inc dec))
(((lambda (x) (x f)) car) 1)
; 2
(((lambda (x) (x f)) cdr) 1)
; 0

; (('thunk 'car) f)
; (('thunk 'cdr) f)
