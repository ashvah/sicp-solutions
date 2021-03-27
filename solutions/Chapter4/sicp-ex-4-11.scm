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
  (env-loop env))

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

(define p (make-frame '(a b c) '(1 2 3)))
(add-binding-to-frame! 'd 4 p)
(define e (cons p '()))
(lookup-variable-value 'd e)
(set-variable-value! 'a '9 e)
e
(define-variable! 'f '10 e)
e
