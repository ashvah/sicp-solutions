#lang sicp

(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess cnt)
    (let ((next (f guess)))
      (display "step")
      (display cnt)
      (display ": ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (inc cnt)))))
  (try first-guess 1))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; step34: 4.555540912917957
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
; step9: 4.5555465521473675
