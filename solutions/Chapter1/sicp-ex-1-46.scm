#lang sicp

(define (iterative-improve good-enough? improve-method)
  (lambda (x)
    (let ((better-guess (improve-method x)))
      (if (good-enough? better-guess)
          better-guess
          ((iterative-improve good-enough? improve-method) better-guess)))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1)
    (< (abs (- v1 (f v1))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  (define tolerance 0.00001)
  (define (close-enough? v1)
    (< (abs (- v1 (/ x v1))) tolerance))
  ((iterative-improve close-enough? (lambda (y) (average y (/ x y)))) 1.0))

(sqrt 2)
; 1.4142156862745097
