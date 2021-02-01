#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponent (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**) (number? (caddr exp))))

(define (exponent exp)
  (caddr exp))

(define (base exp)
  (cadr exp))

(define (make-exponent base exp)
  (cond ((= exp 0) 1)
        ((= exp 1) base)
        (else (list '** base exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (sum? a1) (sum? a2)) (append (list '+) (cdr a1) (cdr a2)))
        ((sum? a1) (append (list '+) (cdr a1) (list a2)))
        ((sum? a2) (append (list '+) (cdr a2) (list a1)))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (product? m1) (product? m2)) (append (list '*) (cdr m1) (cdr m2)))
        ((product? m1) (append (list '*) (cdr m1) (list m2)))
        ((product? m2) (append (list '*) (cdr m2) (list m1)))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((m (cdddr s)))
    (if (pair? m)
        (cons '+ (cddr s))
        (caddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((m (cdddr p)))
    (if (pair? m)
        (cons '* (cddr p))
        (caddr p))))

(deriv '(+ 1 x (* x y) (* x x z) (* x x x 5) (* x 3)) 'x)
; (1+x+xy+x^2z+5x^3+3x)=1+y+2xz+15x^2+3
; (+ (* x z) (* x z) (* x (+ (* x 5) (* x 5))) (* x x 5) 3 y 1)
