#lang sicp

; a)
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (psedoremainder-terms a b))))

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (pseudoremainder-terms)
  (let ((f1 (first-term a))
        (f2 (first-term b)))
    (let ((o1 (order f1))
          (o2 (order f2))
          (c (coeff f2)))
      (remainder-terms (mul a (make-term 0 (exp c (+ 1 (- o1 o2))))) b))))

; Q1: 11x^4-22x^3+18x^2-14x+7
; Q2: 13x^3-21x^2+233x+55
; (gcd-terms Q1 Q2)
; (gcd-terms Q2 (psedoremainder-terms Q1 Q2))
; (gcd-terms Q2 908(x^2-2x+1))
; (gcd-terms 908(x^2-2x+1) (psedoremainder-terms Q2 908(x^2-2x+1)))
; (gcd-terms 908(x^2-2x+1) 0)
; 908(x^2-2x+1)

; b)
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (car (div-terms a (gcd-coeff a)))
      (gcd-terms b (psedoremainder-terms a b))))

(define (gcd-coeff term-list)
  (define (iter term-list result)
    (if (empty-termlist? term-list)
        result
        (let ((c (coeff (first-term term-list)))
              (rests (rest-terms term-list)))
          (iter rests (gcd result c)))))
  (iter (rest-terms term-list) (coeff (first-term term-list))))
