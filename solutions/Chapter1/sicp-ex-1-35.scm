#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; let x=1+1/x
; we get x^2-x-1=0
; x=\frac{1}{2}(1\pm\sqrt{5})
; \phi=\frac{1}{2}(1+\sqrt{5}) is one of the fixed-points of x=>1+1/x

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; 1.6180327868852458
