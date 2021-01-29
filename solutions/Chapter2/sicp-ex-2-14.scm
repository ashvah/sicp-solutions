#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

(define (percent i)
  (/ (width i) (center i)))

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]\n"))

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

(define r1 (make-center-percent 5 0.01))
(define r2 (make-center-percent 10 0.01))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r3 (par1 r1 r2))

(define r4 (par2 r1 r2))

(print-interval r1)
; [4.95, 5.05]
(print-interval r2)
; [9.9, 10.1]
(print-interval r3)
; [3.2346534653465353, 3.4346801346801343]
(print-interval r4)
; [3.3, 3.3666666666666663]
(print-interval (div-interval r1 r1))
; [0.9801980198019803, 1.02020202020202]
; the answer is not something like [1,1]
; that is because the function div-interval always takes x,y as two different intervals
; although we are calculate (div-interval r1 r1)
(print-interval (div-interval r1 r2))
; [0.49009900990099015, 0.51010101010101]
