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

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define a (list (list 1 2 3 4) (list 4 5 6 7) (list 6 7 8 9)))
a
; ((1 2 3 4) (4 5 6 7) (6 7 8 9))
(dot-product (car a) (car (cdr a)))
; (1 2 3 4) * (4 5 6 7) = 60
(matrix-*-vector a (car a))
; ((1 2 3 4) (4 5 6 7) (6 7 8 9))*(1 2 3 4)=(30 60 80)
(matrix-*-vector a (car (cdr a)))
; (60 126 170)
(matrix-*-vector a (car (cdr (cdr a))))
; (80 170 230)
(transpose a)
; ((1 4 6) (2 5 7) (3 6 8) (4 7 9))
(matrix-*-matrix a (transpose a))
; ((30 60 80) (60 126 170) (80 170 230))
