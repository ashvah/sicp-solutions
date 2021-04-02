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

; self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

; quote
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

; variable
(define (variable? exp) (symbol? exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

; assignment

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

; definition
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

; true&false
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; if
(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (force-it (pproc env)))
          (cproc env)
          (aproc env)))))

; unless
(define (unless-pred exp)
  (cadr exp))

(define (unless-alter exp)
  (if (not (null? (cadddr exp)))
      (cadddr exp)
      'false))

(define (unless-cons exp)
  (caddr exp))

(define (unless->if exp)
  (make-if (unless-pred exp) (unless-alter exp) (unless-cons exp)))

(define (analyze-unless exp)
  (analyze (unless->if exp)))

; lambda
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

; begin
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

; apply
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (operator exp)
  (if (tagged-list? exp 'call)
      (cadr exp)
      (car exp)))

(define (operands exp)
  (if (tagged-list? exp 'call)
      (cddr exp)
      (cdr exp)))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (application? exp)
  (pair? exp))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp))))
    (lambda (env)
      (execute-application (force-it (fproc env))
                           (operands exp)
                           env))))

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

(define (analyze-let exp) (analyze (let-combination exp)))

(define (make-let var-exp body)
  (cons 'let (cons var-exp body)))

; eval
(display "\nInstalling eval...\n")
(put 'eval 'quote analyze-quoted)
(put 'eval 'set! analyze-assignment)
(put 'eval 'define analyze-definition)
(put 'eval 'if analyze-if)
(put 'eval 'lambda analyze-lambda)
(put 'eval 'begin analyze-sequence)
(put 'eval 'call analyze-application)
(put 'eval 'let analyze-let)
(put 'eval 'unless analyze-unless)

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((not (pair? exp)) (error "Unknown expression type -- EVAL" exp))
        (else (let ((proc (get 'eval (car exp))))
                (cond (proc (proc exp))
                      ((application? exp) (analyze-application exp))
                      (else (error "Unknown expression type -- ANALYZE" exp)))))))

; procedure
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define apply-procedure
  (lambda (procedure arguments env)
    ((procedure-body procedure)
     (extend-environment (procedure-parameters procedure)
                         (list-of-delayed-args arguments env)
                         (procedure-environment procedure)))))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args env)
  (apply-in-underlying-scheme
   (primitive-implementation proc) (list-of-arg-values args env)))

; apply
(display "\nInstalling apply...\n")
(put 'apply 'procedure apply-procedure)
(put 'apply 'primitive apply-primitive-procedure)

(define (execute-application procedure arguments env)
  (let ((proc (get 'apply (car procedure))))
    (if proc
        (proc procedure arguments env)
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

;--------------------------------------------------------
; thunk

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;---------------------------------------------------------
; UI

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
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

;---------------------------------------------------------
; test
(display "Start testing:\n")
(driver-loop)
