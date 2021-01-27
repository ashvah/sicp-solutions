#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point z)
  (car z))

(define (y-point z)
  (cdr z))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

; 1) defined by two points: bottom-left and top-right points
(define (make-rec1 p0 p1)
  (cons p0 p1))

(define (p0-rec1 s)
  (car s))

(define (p1-rec1 s)
  (cdr s))

(define (len-rec1 s)
  (- (x-point (p1-rec1 s)) (x-point (p0-rec1 s))))

(define (wid-rec1 s)
  (- (y-point (p1-rec1 s)) (y-point (p0-rec1 s))))

(define (S-rec1 s)
  (* (len-rec1 s) (wid-rec1 s)))

(define (C-rec1 s)
  (* 2 (+ (len-rec1 s) (wid-rec1 s))))

(display "implementation  1")
(newline)
(define a (make-point 0 0))
(define b (make-point 3 4))
(define s1 (make-rec1 a b))
(display "bottom-left    ")
(print-point (p0-rec1 s1))
(display "up-right       ")
(print-point (p1-rec1 s1))
(display "perimeter  ")
(C-rec1 s1)
(display "area       ")
(S-rec1 s1)
(newline)

; 2) defined by the bottom-left point, width and length
(define (make-rec2 p0 a b)
  (cons p0 (cons a b)))

(define (p0-rec2 s)
  (car s))

(define (p1-rec2 s)
  (make-point (+ (x-point (p0-rec2 s)) (len-rec2 s)) (+ (y-point (p0-rec2 s)) (wid-rec2 s))))

(define (len-rec2 s)
  (car (cdr s)))

(define (wid-rec2 s)
  (cdr (cdr s)))

(define (S-rec2 s)
  (* (len-rec2 s) (wid-rec2 s)))

(define (C-rec2 s)
  (* 2 (+ (len-rec2 s) (wid-rec2 s))))

(display "implementation  2")
(newline)
(define s2 (make-rec2 a 3 4))
(display "bottom-left    ")
(print-point (p0-rec2 s2))
(display "up-right       ")
(print-point (p1-rec2 s2))
(display "perimeter  ")
(C-rec2 s2)
(display "area       ")
(S-rec2 s2)
