#lang sicp

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

; z1->(*)(*)->(*)(\)
;      |       |
;      |-------+
;      v
;  x->(*)(*)->(*)(\)
;      |       |   
;      v       v       
;     'a      'b

; z2->(*)(*)->(*)(*)->(*)(*)->(*)(\)
;      |       |       |       |
;      v       v       v       v
;     'a      'b      'a      'b

; z1->(*)(*)->(*)(\)
;      |       |
;      |-------+
;      v
;  x->(*)(*)->(*)(\)
;      |       |   
;      v       v       
;    'wow     'b

; z2->(*)(*)->(*)(*)->(*)(*)->(*)(\)
;      |       |       |       |
;      v       v       v       v
;    'wow     'b      'a      'b
