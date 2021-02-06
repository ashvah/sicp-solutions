#lang sicp

(define (type-tag n)
  (cond ((number? n) 'scheme-number)
        ((symbol? n) 'scheme-symbol)
        ((not (pair?)) (error "Bad tagged datum -- TYPE-TAG" n))
        (else (car n))))

(define (attach-tag type n)
  (cond ((eq? type 'scheme-number) n)
        ((eq? type 'scheme-symbol) n)
        (else (cons type n))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((symbol datum) datum)
        ((not (pair? datum)) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'make 'scheme-number
       (lambda (x) x))
  'done)
