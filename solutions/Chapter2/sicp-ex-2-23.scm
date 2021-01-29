#lang sicp

(define (for-each f x)
  (cond ((not (null? x)) (f (car x))
                         (for-each f (cdr x)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
