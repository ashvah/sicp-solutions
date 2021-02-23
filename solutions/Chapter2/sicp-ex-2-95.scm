#lang sicp

; Q1: 11x^4-22x^3+18x^2-14x+7
; Q2: 13x^3-21x^2+233x+55
; (gcd-terms Q1 Q2)
; (gcd-terms Q2 (remainder-terms Q1 Q2))
; (gcd-terms Q2 908/169(x^2-2x+1))
; (gcd-terms 908/169(x^2-2x+1) (remainder-terms Q2 908/169(x^2-2x+1)))
; (gcd-terms 908/169(x^2-2x+1) 0)
; 908/169(x^2-2x+1) .....

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (let ((f1 (first-term a))
            (f2 (first-term b)))
        (let ((o1 (order f1))
              (o2 (order f2))
              (c (coeff f2)))
          (gcd-terms b (remainder-terms (mul a (make-term 0 (power c (+ 1 (- o1 o2))))) b))))))
