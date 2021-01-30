#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))


(map inc (list 1 2 3 4 5))
; (2 3 4 5 6)
(map (lambda (x) (* x x)) (list 1 2 3 4 5))
; (1 4 9 16 25)
(append (list 1 2 4 5) (list 9 8 5 3))
; (1 2 4 5 9 8 5 3)
(length (list 3 4 2 4))
; 4
