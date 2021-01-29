#lang sicp

(define (last-pair x)
  (let ((y (cdr x)))
       (if (null? y)
           x
           (last-pair y))))

(define (reverse-by-append x)
  (let ((y (cdr x)))
    (if (null? y)
        x
        (append (reverse-by-append (cdr x)) (cons (car x) nil)))))


(define (reverse-by-iter x)
  (define (iter x result)
    (if (null? x)
        result
        (iter (cdr x) (cons (car x) result))))
  (iter x nil))

(reverse-by-append (list 23 72 149 34))
; (34 149 72 23)
(reverse-by-iter (list 23 72 149 34))
; (34 149 72 23)
