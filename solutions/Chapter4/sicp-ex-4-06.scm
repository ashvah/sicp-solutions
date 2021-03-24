#lang sicp

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let-vars exp)
  (map car (cadr exp)))

(define (let-exps exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (cddr exp))

(define (let-combination exp)
  (cons 'call (cons (make-lambda (let-vars exp) (let-body exp)) (let-exps exp))))

(define eval-let
  (lambda (exp env) (eval (let-combination exp) env)))

(put 'eval 'let let-combination)

(let-combination '(let ((x 1) (y 2)) (+ x y)))
; (call (lambda (x y) (+ x y)) 1 2)
