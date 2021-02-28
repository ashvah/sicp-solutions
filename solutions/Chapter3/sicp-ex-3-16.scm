#lang sicp

(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z1 (cons x y))
z1
; z1->(*)(*)
;      |  |
;      |  +----------+
;      v             |
;  x->(*)(*)     y->(*)(*)  
;      |  |          |  |
;      v  v          v  v
;     'a 'b         'c 'd

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs z1)
; 3

(set-cdr! x y)
(define z2 (cons x y))
z2
; z2->(*)(*)
;      |  |
;      |  +----------+
;      v             |
;  x->(*)(*)     y->(*)(*)   
;      |  |      ^   |  |
;      v  |      |   v  v
;     'a  +------+  'c 'd
(count-pairs z2)
; 4

(set-car! x y)
(define z3 (cons x x))
z3
; z3->(*)(*)
;      |  |
;      |--+
;      v         
;  x->(*)(*)     y->(*)(*)   
;      |  |      ^   |  |
;      |  |      |   v  v
;      +--+------+  'c 'd
(count-pairs z3)
; 7

(set-car! x 'a)
(set-car! y x)
(define z4 (cons x x))
z4
; z4->(*)(*)
;      |  |
;      |  |
;      |--+-------------+
;      v                |
;  x->(*)(*)     y->(*)(*)   
;      |  |      ^   |  
;      |  |      |   v  
;     'a  +------+  'c 
(count-pairs z4)
; infinite
