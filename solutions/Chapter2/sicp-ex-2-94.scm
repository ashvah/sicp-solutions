#lang sicp

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (let ((v1 (variable p1))
        (v2 (variable p2)))
    (if (same-variable? v1 v2)
        (gcd-terms (term-list p1) (term-list p2))
        (error "Different variables --GCD-POLY" (list p1 p2)))))

(put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))

(put 'gcd '(scheme-number scheme-number) (lambda (a b) (tag (gcd a b))))

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))
