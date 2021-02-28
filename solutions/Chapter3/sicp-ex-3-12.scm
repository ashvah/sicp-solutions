#lang sicp

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))

; x->(*)(*)->(*)(\)
;     |       |
;     v       v
;    'a      'b

(define y (list 'c 'd))

; y->(*)(*)->(*)(\)
;     |       |
;     v       v
;    'c      'd

(define z (append  x y))

; z->(*)(*)->(*)(*)->(*)(*)->(*)(\)
;     |       |       |       |
;     v       v       v       v
;    'a      'b      'c      'd

z
; (a b c d)

(cdr x)
; (b)
;  ->(*)(\)
;     |       
;     v       
;    'b

(define w (append! x y))

; x,w->(*)(*)->(*)(*)->(*)(*)->(*)(\)
;       |       |       |       |
;       v       v       v       v
;      'a      'b      'c      'd

w
; (a b c d)

(cdr x)
; (b c d)

; x,w->(*)(*)->(*)(*)->(*)(\)
;       |       |       |       
;       v       v       v       
;      'b      'c      'd
