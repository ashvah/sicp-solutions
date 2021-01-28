#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "illegal")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define a (make-interval 1 2))
(define b (make-interval 3 4))
(define c (make-interval -3 4))
(define d (make-interval -4 -4))
(define e (make-interval 0 9))
(div-interval a b)
; (0.25 . 0.6666666666666666)
(div-interval a d)
; (-0.5 . -0.25)
(div-interval a c)
; illegal
(div-interval a e)
; illegal
