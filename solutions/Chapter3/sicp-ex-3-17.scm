#lang sicp

(define (count-pairs x)
  (define (find list-of-old y)
    (if (null? list-of-old)
        #f
        (if (eq? (car list-of-old) y)
            #t
            (find (cdr list-of-old) y))))
  (define (iter list-to-count list-of-old)
    (if (not (pair? list-to-count))
        0
        (let ((new-car (find list-of-old (car list-to-count)))
              (new-cdr (find list-of-old (cdr list-to-count))))
          (if new-car
              (if new-cdr
                  1
                  (+ 1 (iter (cdr list-to-count) (cons (cdr list-to-count) list-of-old))))
              (if new-cdr
                  (+ 1 (iter (car list-to-count) (cons (car list-to-count) list-of-old)))
                  (if (eq? (car list-to-count) (cdr list-to-count))
                      (+ 1 (iter (car list-to-count) (cons (car list-to-count) list-of-old)))
                      (+ 1
                         (iter (car list-to-count) (cons (car list-to-count) (cons (cdr list-to-count) list-of-old)))
                         (iter (cdr list-to-count) (cons (car list-to-count) (cons (cdr list-to-count) list-of-old))))))))))
  (iter x (list x)))

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
