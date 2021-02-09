#lang sicp

(define (raise-scheme-number n)
  (make-rational n 1))

(define (raise-rational n)
  (make-real (/ (numer n) (denom n))))

(define (raise-real n)
  (make-from-real-imag n 0))

(put 'raise 'scheme-number raise-scheme-number)

(put 'raise 'ration raise-ration)

(put 'raise 'real raise-real)

(define (raise n)
  (apply-generic 'raise n))
