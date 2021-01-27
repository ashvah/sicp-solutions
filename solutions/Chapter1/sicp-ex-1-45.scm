#lang sicp
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define (exp base n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
          ((= (remainder n 2) 0) (square (exp base (/ n 2))))
          (else (* base (exp base (- n 1))))))

(define (n-root x n)
  (define (lg x)
    (if (<= x 1)
        0
        (inc (lg (/ x 2)))))
  (fixed-point ((repeated average-damp (lg n)) (lambda (y) (/ x (exp y (- n 1))))) 1.0))

(define (check x n)
  (abs (- (exp (n-root x n) n) x)))

(check 2 1)
(check 2 2)
(check 2 3)
(check 2 4)
(check 2 5)
