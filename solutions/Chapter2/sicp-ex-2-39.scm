#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reverse-r seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse-l seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

(reverse-r (list 1 2 3 4))
;(4 3 2 1)
(reverse-l (list 1 2 3 4))
;(4 3 2 1)
