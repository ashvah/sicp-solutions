#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
; (/ 1 (fold-right / 1 (list 2 3)))
; (/ 1 (/ 2 (fold-right / 1 (list 3))))
; (/ 1 (/ 2 (/ 3 1)))
; 3/2

(fold-left / 1 (list 1 2 3))
; (iter 1 (list 1 2 3))
; (iter (/ 1 1) (list 2 3))
; (iter (/ 1 2) (list 3))
; (iter (/ 1/2 3) nil)
; 1/6

(fold-right list nil (list 1 2 3))
; (list 1 (fold-right list nil (list 2 3)))
; (list 1 (list 2 (fold-right list nil (list 3))))
; (list 1 (list 2 (list 3 nil)))
; (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
; (iter nil (list 1 2 3))
; (iter (list nil 1) (list 2 3))
; (iter (list (list () 1) 2) (list 3))
; (iter (list (list (list () 1) 2) 3) nil)
; (((() 1) 2) 3)

; (op a (op b ... (op x init) ...))=(op (op (op init a) b) ... x)
; (op a init)=(op init a)
; commutative   a op b =b op a
; (op a (op b init))=(op (op init a) b)=(op b (op a init))
; associative   (a op (b op c))=(b op (a op c))
