#lang sicp

(define (equal? a b)
  (or (and (pair? a) (pair? b) (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (and (not (pair? a)) (not (pair? b)) (eq? a b))))

(equal? '(this is a list) '(this is a list))
; #t
(equal? '(this is a list) '(this (is a) list))
; #f
