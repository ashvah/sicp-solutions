#lang sicp

(define (install-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (install-rational-package)
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  'done)
       
(define (install-complex-package)
  (put 'equ? 'complex
       (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  'done)

(define (equ? x y)
  (let ((x-tag (type-tag x))
        (y-tag (type-tag y)))
    (if (eq? x-tag y-tag)
        (apply-generic equ x y)
        false)))
