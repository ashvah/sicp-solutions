#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(a b c d))

; v->(*)(*)->(*)(*)->(*)(*)->(*)(\)
;     |       |       |       |
;     v       v       v       v
;    'a      'b      'c      'd

(define w (mystery v))

;                             v
;                             |
;                             v
; w->(*)(*)->(*)(*)->(*)(*)->(*)(\)
;     |       |       |       |
;     v       v       v       v
;    'd      'c      'b      'a

v
; (a)
w
; (d c b a)
