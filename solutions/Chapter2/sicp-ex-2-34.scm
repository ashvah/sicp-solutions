#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 1.0 (list 1 2 3 4))
; 10.0=1+2+3+4
(horner-eval 2.0 (list 1 2 3 4))
; 49.0=1+2*2+3*4+4*8
(horner-eval 2 (list 1 3 0 5 0 1))
; 79=1+3*2+5*8+1*32
