#lang sicp

;---------------------------------------------------------

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

;---------------------------------------------------------------
; and & or

(define (get-clauses exp) (cdr exp))

(define (first-clause clauses) (car clauses))

(define (rest-clauses clauses) (cdr clauses))

(define (eval-and-clauses clauses env)
  (if (null? clauses)
      'true
      (let ((result (eval (first-clause clauses)env)))
        (if (eq? result 'false)
            'false
            (eval-and-clause (rest-clauses clauses) env)))))

(define (eval-or-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((result (eval (first-clause clauses)env)))
        (if (eq? result 'true)
            'true
            (eval-and-clause (rest-clauses clauses) env)))))

(define eval-and
  (lambda (exp env)
    (eval-and-clauses (get-clauses exp) env)))

(define eval-or
  (lambda (exp env)
    (eval-or-clauses (get-clauses exp) env)))

(put 'eval 'and eval-and)
(put 'eval 'or eval-or)
