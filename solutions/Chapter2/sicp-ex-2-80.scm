#lang sicp

(define (install-scheme-number-package)
  (put '=zero? '(scheme-number scheme-number)
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-package)
  (put '=zero '(rational rational)
       (lambda (x) (= (numer x) 0)))
  'done)
       
(define (install-complex-package)
  (put '=zero? 'complex
       (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  'done)

(define (=zero? x)
  (apply-generic '=zero? x))
