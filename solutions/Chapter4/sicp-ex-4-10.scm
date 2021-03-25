#lang sicp

; (if (predicate if-consequent if-alternative))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (caadr exp))

(define (if-consequent exp) (cadadr exp))

(define (if-alternative exp)
  (if (not (null? (cddadr exp)))
      (car (cddadr exp))
      'false))

(define (make-if predicate consequent alternative)
  (list 'if (list predicate consequent alternative)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
