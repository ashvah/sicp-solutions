#lang sicp

;---------------------------------------------------------
; table

(display "Initializing...\n")

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
      (display key-1)
      (display ": ")
      (display key-2)
      (display "\t\tdone\n"))    
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

(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (application? exp)
  (pair? exp))

(define eval-call
  (lambda (exp env)
    (apply-new (eval (operator exp) env)
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

; true&false
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

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

(define (cond->if exp env)
  (expand-clauses (cond-clauses exp) env))

(define (cond-test-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (cond-recipient clause)
  (cadr (cond-actions clause)))

(define (expand-clauses clauses env)
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
                (let ((result (eval (cond-predicate first) env)))
                  (make-if result
                           (list 'call (cond-recipient first) 'test-variable)
                           (expand-clauses rest env)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest env)))))))

(define eval-cond
  (lambda (exp env)
    (eval (cond->if exp env) env)))

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
  (if (pair? (cadr exp))
      (map car (cadr exp))
      (map car (caddr exp))))

(define (let-exps exp)
  (if (pair? (cadr exp))
      (map cadr (cadr exp))
      (map cadr (caddr exp))))

(define (let-body exp)
  (if (pair? (cadr exp))
      (cddr exp)
      (cdddr exp)))

(define (let-combination exp)
  (if (pair? (cadr exp))
      (cons 'call (cons (make-lambda (let-vars exp) (let-body exp)) (let-exps exp)))
      (list (make-lambda '()
            (list (list 'define (cadr exp) (make-lambda (let-vars exp) (let-body exp)))
                  (cons 'call (cons (cadr exp) (let-exps exp))))))))

(define eval-let
  (lambda (exp env) (eval (let-combination exp) env)))

(define (make-let var-exp body)
  (cons 'let (cons var-exp body)))

; let*
(define (let*->nested-lets exp)
  (define (let*->nested-lets-iter var-exp body)
    (cond ((null? var-exp) (make-let '() body))
          ((null? (cdr var-exp)) (make-let (list (car var-exp)) body))
          (else (make-let (list (car var-exp))
                          (list (let*->nested-lets-iter (cdr var-exp) body))))))
  (let*->nested-lets-iter (cadr exp) (let-body exp)))

(define eval-let*
  (lambda (exp env) (eval (let*->nested-lets exp) env)))

; letrec
(define (letrec->let exp)
  (let ((vars (let-vars exp))
        (exps (let-exps exp))
        (body (let-body exp)))
    (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
              (append (map (lambda (x y) (list 'set! x y)) vars exps) body))))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

; eval
(display "\nInstalling eval...\n")
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
(put 'eval 'let eval-let)
(put 'eval 'let* eval-let*)
(put 'eval 'letrec eval-letrec)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((not (pair? exp)) (error "Unknown expression type -- EVAL" exp))
        (else (let ((proc (get 'eval (car exp))))
                (cond (proc (proc exp env))
                      ((application? exp) (apply-new (eval (car exp) env)
                                                 (list-of-values (cdr exp) env)))
                      (else (error "Unknown expression type -- EVAL" exp)))))))

; procedure
(define (scan-out-defines body)
  (define (find-def-list exp-list)
    (if (null? exp-list)
        (cons '() '())
        (let ((rest (find-def-list (cdr exp-list))))
          (if (not (tagged-list? (car exp-list) 'define))
              (cons (car rest) (cons (car exp-list) (cdr rest)))
              (cons (cons (definition-variable (car exp-list)) (car rest))
                    (cons (cons 'set! (cons (definition-variable (car exp-list)) (cons (definition-value (car exp-list)) nil))) (cdr rest)))))))
  (let ((def-list (find-def-list body)))
    (if (null? (car def-list))
        body
        (list (make-let (map (lambda (x) (list x ''*unassigned*)) (car def-list))
                        (cdr def-list))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define apply-procedure
  (lambda (procedure arguments)
    (eval-sequence (procedure-body procedure)
                   (extend-environment (procedure-parameters procedure)
                                       arguments
                                       (procedure-environment procedure)))))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; apply
(display "\nInstalling apply...\n")
(put 'apply 'procedure apply-procedure)
(put 'apply 'primitive apply-primitive-procedure)

(define (apply-new procedure arguments)
  (let ((proc (get 'apply (car procedure))))
    (if proc
        (proc procedure arguments)
        (error "Unknown procedure type -- APPLY" procedure))))

;-------------------------------------------------------------------------------
; environment

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame-list)
      (cond ((null? frame-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car frame-list)))
             (cdr (car frame-list)))
            (else (scan (cdr frame-list)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (let ((result (env-loop env)))
    (if (eq? result '*unassigned*)
        (error "Variable unassigned --LOOKUP-VARIABLE-VALUE" var)
        result)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame-list)
      (cond ((null? frame-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car frame-list)))
             (set-car! frame-list (cons var val)))
            (else (scan (cdr frame-list)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame-list)
      (cond ((null? frame-list)
             (add-binding-to-frame! var val frame))
            ((eq? var (car (car frame-list)))
             (set-car! frame-list (cons var val)))
            (else (scan (cdr frame-list)))))
    (scan (cdr frame))))

(define (traverse env)
  (map (lambda (x)
         (map (lambda (pair)
                (display (car pair))
                (display "\t")
                (display (cdr pair))
                (newline))
              (cdr x)))
       env)
  (display "end\n"))

;---------------------------------------------------------
; UI

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (compound-procedure? x) (tagged-list? x 'procedure))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;---------------------------------------------------------------
; initialize

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
        (list 'inc inc)
        ; add more primitive procedures here
        ))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(display "\nInitializing environment...\n")

(traverse the-global-environment)
(display "\nInitialized!\n\n")
(display "--------Applicative Order v1--------\n\n")

;---------------------------------------------------------
; test
(display "Start testing:\n")
(driver-loop)
