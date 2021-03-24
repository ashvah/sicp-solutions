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

(define (make-let var-exp body)
  (cons 'let (cons var-exp body)))

(define (let*->nested-lets exp)
  (define (let*->nested-lets-iter var-exp body)
    (cond ((null? var-exp) (make-let '() body))
          ((null? (cdr var-exp)) (make-let (list (car var-exp)) body))
          (else (make-let (list (car var-exp))
                          (list (let*->nested-lets-iter (cdr var-exp) body))))))
  (let*->nested-lets-iter (cadr exp) (let-body exp)))

(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
; (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
(let*->nested-lets '(let* () (* x z)))
; (let () (* x z))
