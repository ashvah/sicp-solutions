#lang sicp

; explicit dispatch allows to add new operations in the same manner
; (define (make-xx args) (...))
; (define (op1-xx args) (...))
; (define (op2-xx args) (...))
; (define (op1 args)
;   (cond ((tag_is_xx? args) (op1-xx (contents args)))
;         (...)))
; (define (op2 args)
;   (cond ((tag_is_xx? args) (op2-xx (contents args)))
;         (...)))

; data-directed approach
; (define (install-pakage-xx)
;   (define op1 ...)
;   (put 'op1 '(xx) op1)
;   ...)
; add new entries in the dispatch table

; message-passing: add new types
; (define (make-xx args)
;   (define (dispatch op)
;      (cond ((eq? op op1) (...))
;            ((eq? op op2) (...))))
;    op)
