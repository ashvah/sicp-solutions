#lang sicp

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

(define (env-loop env var proc1 proc2)
  (define (scan frame frame-list)
    (cond ((null? frame-list)
           ((proc1 frame frame-list) (enclosing-environment env) var proc1 proc2))
          ((eq? var (car (car frame-list)))
           (proc2 frame-list))
          (else (scan frame (cdr frame-list)))))
  (if (eq? env the-empty-environment)
      (error "fail --env-loop" var)
      (let ((frame (first-frame env)))
        (scan frame (cdr frame)))))

(define (lookup-variable-value var env)
  (env-loop env
            var
            (lambda (x y) env-loop)
            (lambda (x) (cdr (car x)))))

(define (set-variable-value! var val env)
  (env-loop env
            var
            (lambda (x y) env-loop)
            (lambda (x) (set-car! x (cons var val)))))

(define (define-variable! var val env)
  (env-loop env
            var
            (lambda (x y) (lambda (env var proc1 proc2) (add-binding-to-frame! var val x)))
            (lambda (env var proc1 proc2) (add-binding-to-frame!))))

(define p (make-frame '(a b c) '(1 2 3)))
(add-binding-to-frame! 'd 4 p)
(define e (cons p '()))
(lookup-variable-value 'd e)
(set-variable-value! 'a '9 e)
e
(define-variable! 'f '10 e)
e
