#lang sicp

(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00005)

; a)
; f=1/(1+f)
; f^2+f=1
; f=\frac{-1+\sqrt{5}}{2}=1/phi

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (compute-phi)
  (define true-value 0.618033988)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try n)
    (let ((phi (cont-frac (lambda (i) 1.0)
                          (lambda (i) 1.0)
                          n)))
      (display phi)
      (newline)
      (if (close-enough? true-value phi)
          n
          (try (+ n 1)))))
  (try 1))

(compute-phi)
; k>=11

; b) iterative
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (compute-phi-iter)
  (define true-value 0.618033988)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try n)
    (let ((phi (cont-frac-iter (lambda (i) 1.0)
                          (lambda (i) 1.0)
                          n)))
      (display phi)
      (newline)
      (if (close-enough? true-value phi)
          n
          (try (+ n 1)))))
  (try 1))

(compute-phi-iter)
