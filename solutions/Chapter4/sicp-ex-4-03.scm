#lang sicp

(define eval-quote
  (lambda (exp env)
    (text-of-quotation exp)))

(define eval-lambda
  (lambda (exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env)))

(define eval-begin
  (lambda (exp env)
    (eval-sequence (begin-actions exp) env)))

(define eval-cond
  (lambda (exp env)
    (eval (cons->if exp) env)))

(define eval-call
  (lambda (exp env)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env))))

(put 'eval 'quote eval-quote)
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda eval-lambda)
(put 'eval 'begin eval-begin)
(put 'eval 'cond eval-cond)
(put 'eval 'call eval-call)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (let ((proc (get 'eval (operator exp))))
                (if proc
                    (proc exp env)
                    (error "Unknown expression type -- EVAL" exp))))))
