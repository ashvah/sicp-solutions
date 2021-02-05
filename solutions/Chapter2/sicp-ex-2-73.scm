#lang sicp

; a)
; predicates, like number? and same-variable?, don't have tags or contents.
; There is no need to put them into the table
;
; b)
(define (install-add-deriv)
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (put 'deriv '(+) (lambda (exp var) (make-sum (deriv (addend exp) var)
                                                    (deriv (augend exp) var))))
  'done)

(define (install-product-deriv)
  (define (make-product m1 m2) (list '* m1 m2))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (put 'deriv '(*) (lambda (exp var) (make-sum
                                           (make-product (multiplier exp)
                                                         (deriv (multiplicand exp) var))
                                           (make-product (deriv (multiplier exp) var)
                                                         (multiplicand exp)))))
  'done)
;
; c)
(define (install-product-deriv)
  (define (make-product m1 m2) (list '* m1 m2))
  (define (exponent exp) (cadr exp))
  (define (base exp) (car exp))
  (define (make-exponent base exp)
    (cond ((= exp 0) 1)
          ((= exp 1) base)
          (else (list '** base exp))))

  (put 'deriv '(**) (lambda (exp var) (make-product (make-product (exponent exp)
                                                                      (make-exponent (base exp) (- (exponent exp) 1)))
                                                        (deriv (base exp) var)))))
; d)
(put (+) 'deriv ...)
(put (*) 'deriv ...)
(put (**) 'deriv ...)
