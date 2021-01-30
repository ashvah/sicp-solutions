#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves s)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1)) s)))

(list 1 (list 1 2 (list 3 4) 2) 2 (list 7 8))
; (1 (1 2 (3 4) 2) 2 (7 8))
(count-leaves (list 1 (list 1 2 (list 3 4) 2) 2 (list 7 8)))
; 9
