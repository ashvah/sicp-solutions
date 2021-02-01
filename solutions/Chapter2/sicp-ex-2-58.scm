#lang sicp

(define (deriv exp var)
  (deriv-new (pre-process exp) var))

(define (deriv-new exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-new (multiplicand exp) var))
           (make-product (deriv-new (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (sum? a1) (sum? a2)) (append a1 (list '+) a2))
        ((sum? a1) (append a1 (list '+) (list a2)))
        ((sum? a2) (append a2 (list '+) (list a1)))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (product? m1) (product? m2)) (append m1 (list '*) (cdr m2)))
        ((product? m1) (append m1 (list '*) (list m2)))
        ((product? m2) (append m2 (list '*) (list m1)))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (let ((m (cdddr s)))
    (if (null? m)
        (caddr s)
        (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
  (let ((m (cdddr p)))
    (if (null? m)
        (caddr p)
        (cddr p))))

(define (pre-process exp)
  (cond ((and (pair? exp)(<= (length exp) 1)) (car exp))
        ((not (pair? exp)) exp)
        ((eq? (cadr exp) '+) (list (pre-process (car exp)) '+ (pre-process (cddr exp))))
        ((and (pair? exp)(= (length exp) 3)) (list (pre-process (car exp)) '* (pre-process (caddr exp))))
        (else (pre-process (cons (list (pre-process (car exp)) '* (pre-process (caddr exp))) (cdddr exp))))))


(deriv '(x + x * (2 * x * y + 3 + z)) 'x)
; (((2 * x) * y) + (3 + z) + (2 * y * x) + 1)
(deriv (deriv '(x + (x * ((2 * x * y) + 3 + z))) 'x) 'x)
; ((2 * y) + (2 * y))
(deriv (deriv (deriv '(x + (x * ((2 * x * y) + 3 + z))) 'x) 'x) 'x)
; 0
