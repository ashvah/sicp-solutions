#lang sicp

(define (count-pairs x)
  (let ((list-counted '()))
    (define (iter list-to-count)
      (if (or (not (pair? list-to-count)) (memq list-to-count list-counted))
          0
          (begin
            (set! list-counted (cons list-to-count list-counted))
            (+ (iter (car list-to-count))
               (iter (cdr list-to-count))
               1))))
    (iter x)))

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
; 3

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
; 3

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
; 3
