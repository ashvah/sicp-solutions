#lang sicp

(define (integral integrand-delay initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force integrand-delay)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))
                               
