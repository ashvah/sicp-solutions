#lang sicp

(define (square x) (* x x))

(define (fast-exp base exp)
  (cond ((= exp 0) 1)
        ((= (remainder exp 2) 0) (square (fast-exp base (/ exp 2))))
        (else (* base (fast-exp base (- exp 1))))))

(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define z (cons 17 43))
(display z)
(newline)
; 2^7*3^4=128*81=10368

(define (car z)
  (if (= (remainder z 2) 1)
      0
      (inc (car (/ z 2)))))

(car z)
; 17

(define (cdr z)
  (if (= (remainder z 3) 0)
      (inc (cdr (/ z 3)))
      0))

(cdr z)
; 43
