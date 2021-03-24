#lang sicp

;---------------------------------------------------------
; table

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;---------------------------------------------------------
; core

; call
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define eval-call
  (lambda (exp env)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env))))

; if
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define eval-begin
  (lambda (exp env)
    (eval-sequence (begin-actions exp) env)))

; set!
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; define
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; quote
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define eval-quote
  (lambda (exp env)
    (text-of-quotation exp)))

; variable
(define (variable? exp) (symbol? exp))

; lambda
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define eval-lambda
  (lambda (exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env)))

;cond
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (cond-test-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (cond-recipient clause)
  (cadr (cond-actions clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                         
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (cond-test-clause? first)
                (list 'call
                      (make-lambda 'test-variable
                                  (list (make-if 'test-variable
                                                 (list 'call (cond-recipient first) 'test-variable)
                                                 (expand-clauses rest))))
                      (cond-predicate first))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define eval-cond
  (lambda (exp env)
    (eval (cond->if exp) env)))


; and-or
(define (get-clauses exp) (cdr exp))

(define (first-clause clauses) (car clauses))

(define (rest-clauses clauses) (cdr clauses))

(define (eval-and-clauses clauses env)
  (if (null? clauses)
      'true
      (let ((result (eval (first-clause clauses)env)))
        (if (eq? result 'false)
            'false
            (eval-and-clauses (rest-clauses clauses) env)))))

(define (eval-or-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((result (eval (first-clause clauses)env)))
        (if (eq? result 'true)
            'true
            (eval-and-clauses (rest-clauses clauses) env)))))

(define eval-and
  (lambda (exp env)
    (eval-and-clauses (get-clauses exp) env)))

(define eval-or
  (lambda (exp env)
    (eval-or-clauses (get-clauses exp) env)))

; let
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

; install
(put 'eval 'quote eval-quote)
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda eval-lambda)
(put 'eval 'begin eval-begin)
(put 'eval 'cond eval-cond)
(put 'eval 'call eval-call)
(put 'eval 'and eval-and)
(put 'eval 'or eval-or)
(put 'eval 'let let-combination)

; eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((not (pair? exp)) (error "Unknown expression type -- EVAL" exp))
        (else (let ((proc (get 'eval (car exp))))
                (if proc
                    (proc exp env)
                    (error "Unknown expression type -- EVAL" exp))))))
