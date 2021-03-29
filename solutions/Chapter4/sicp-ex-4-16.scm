#lang sicp

; a)
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

; b)
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-let var-exp body)
  (cons 'let (cons var-exp body)))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

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

(scan-out-defines (lambda-body '(define (f x)
                                  (define (even? n)
                                    (if (= n 0)
                                        true
                                        (odd? (- n 1))))
                                  (define (odd? n)
                                    (if (= n 0)
                                        false
                                        (even? (- n 1))))
                                  (e_1)
                                  (define (g x) (+ x 1))
                                  (e_2)
                                  )))

; ((let ((even? '*unassigned*) (odd? '*unassigned*) (g '*unassigned*))
;   (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
;   (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
;   (e_1)
;   (set! g (lambda (x) (+ x 1)))
;   (e_2)))

; c) make-procudure

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
