#lang sicp

(define (fringe-by-append x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe-by-append (car x)) (fringe-by-append (cdr x))))))

(define (fringe-by-iter x)
  (define (iter y result)
    (cond ((null? y) result)
          ((not (pair? y)) (cons y result))
          (else (let ((u (car y))
                      (v (cdr y)))
                  (iter u (iter v result))))))
  (iter x nil))


(define x (list (list 1 2) (list 3 4)))
(fringe-by-append x)
; (1 2 3 4)
(fringe-by-append (list x x))
; (1 2 3 4 1 2 3 4)
(fringe-by-iter x)
; (1 2 3 4)
(fringe-by-iter (list x x))
; (1 2 3 4 1 2 3 4)
