#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (cond ((and (<= (upper-bound x) 0)
              (<= (upper-bound y) 0)) (make-interval (* (upper-bound x) (upper-bound y))
                                                     (* (lower-bound x) (lower-bound y))))
        ((and (>= (lower-bound x) 0)
              (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (lower-bound y))
                                                     (* (upper-bound x) (upper-bound y))))
        ((and (<= (upper-bound x) 0)
              (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (upper-bound y))
                                                     (* (upper-bound x) (lower-bound y))))
        ((and (>= (lower-bound x) 0)
              (<= (upper-bound y) 0)) (make-interval (* (upper-bound x) (lower-bound y))
                                                     (* (lower-bound x) (upper-bound y))))
        ((and (>= (lower-bound x) 0)
              (<= (lower-bound y) 0)
              (>= (upper-bound y) 0)) (make-interval (* (upper-bound x) (lower-bound y))
                                                     (* (upper-bound x) (upper-bound y))))
        ((and (<= (upper-bound x) 0)
              (<= (lower-bound y) 0)
              (>= (upper-bound y) 0)) (make-interval (* (lower-bound x) (upper-bound y))
                                                     (* (lower-bound x) (lower-bound y))))
        ((and (>= (lower-bound y) 0)
              (<= (lower-bound x) 0)
              (>= (upper-bound x) 0)) (make-interval (* (upper-bound y) (lower-bound x))
                                                     (* (upper-bound y) (upper-bound x))))
        ((and (<= (upper-bound y) 0)
              (<= (lower-bound x) 0)
              (>= (upper-bound x) 0)) (make-interval (* (lower-bound y) (upper-bound x))
                                                     (* (lower-bound y) (lower-bound x))))
        (else (make-interval (min (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
                             (max (* (lower-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))))))

(define (check x y)
  (let ((old (old-mul-interval x y))
        (new (mul-interval x y)))
    (let ((x1 (lower-bound old))
          (x2 (lower-bound new))
          (y1 (upper-bound old))
          (y2 (upper-bound new)))
      (if (and (= x1 x2)
               (= y1 y2))
          (display "true\n")
          (display "false\n")))))

(define a (make-interval 1 2))
(define b (make-interval -3 -4))
(define c (make-interval -7 4))
(check a a)
(check a b)
(check a c)
(check b a)
(check b b)
(check b c)
(check c a)
(check c b)
(check c c)
; true
; true
; true
; true
; true
; true
; true
; true
; true
