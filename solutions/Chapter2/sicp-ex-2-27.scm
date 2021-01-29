#lang sicp

(define (deep-reverse x)
  (define (iter y result)
    (cond ((null? y) result)
          (else (let ((u (deep-reverse (car y)))
                      (v (cdr y)))
                  (iter v (cons u result))))))
  (if (not (pair? x))
      x
      (iter x nil)))

(define x (list (list 1 2) (list 3 4)))
x
; ((1 2) (3 4))
(reverse x)
; ((3 4) (1 2))
(deep-reverse x)
; ((4 3) (2 1))

(define y (list (list 1 (list 2 3 5) 3) (list 3 4)))
y
; ((1 (2 3 5) 3) (3 4))
(reverse y)
; ((3 4) (1 (2 3 5) 3))
(deep-reverse y)
; ((4 3) (3 (5 3 2) 1))
