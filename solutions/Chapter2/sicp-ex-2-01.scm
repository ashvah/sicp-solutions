#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (define positive?
    (or (and (< n 0) (< d 0))
        (and (> n 0) (> d 0))))
  (let ((g (gcd (abs n) (abs d))))
    (if positive?
        (cons (/ (abs n) g) (/ (abs d) g))
        (cons (- (/ (abs n) g)) (/ (abs d) g)))))

(define (denom x)
  (cdr x))

(define (numer x)
  (car x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define a (make-rat 1 2))
(define b (make-rat -4 4))
(define c (make-rat -8 -5))
(define d (make-rat 3 -7))
(print-rat a)
; 1/2
(print-rat b)
; -1/1
(print-rat c)
; 8/5
(print-rat d)
; -3/7
(print-rat (add-rat a d))
; 1/14
