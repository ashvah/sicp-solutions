#lang sicp

;----------------------------------------
; definition of natural numbers

(define zero-nat
  (lambda (f x) x))

(define (succ-nat n)
  (lambda (f x)
    (f (n f x))))

(define (add-nat a b)
  (lambda (f x)
    (a f (b f x))))

(define (pred-nat n)
  (lambda (f x)
    ((n (lambda (g) (lambda (h) (h (g f))))
        (lambda (u) x))
     (lambda (u) u))))

(define (sub-nat a b)
  (b pred-nat a))

(define (mul-nat a b)
  (lambda (f x)
    (a (lambda (x) (b f x)) x)))

(define (exp-nat a b)
  (lambda (f x) ((b (lambda (f) (lambda (x) (a f x))) f) x)))

(define (display-nat n) (n inc 0))

; test

(display-nat zero-nat)
; 0 
(define five-nat (add-nat (succ-nat (succ-nat (succ-nat zero-nat))) (succ-nat (succ-nat zero-nat))))
(display-nat five-nat)
; 5
(define four-nat (pred-nat five-nat))
(display-nat four-nat)
; 4
(define one-nat (sub-nat five-nat four-nat))
(display-nat one-nat)
; 1
(define twenty-nat (mul-nat four-nat five-nat))
(display-nat twenty-nat)
; 20
(display-nat (exp-nat five-nat four-nat))
; 625

;------------------------------------------
; definition of boolean

(define true
  (lambda (t f)
    t))

(define false
  (lambda (t f)
    f))

(define (test e1 e2 e3)
  (e1 e2 e3))

(define (and a b)
  (a b false))

(define (or a b)
  (a true b))

(define (iszero? n)
  (n (lambda (f) false) true))

(define (equal-nat? x y)
  (and (iszero? (sub-nat  x y)) (iszero? (sub-nat y x))))

(define (display-bool x) (test x 'true 'false))

; test

(display-bool (iszero? zero-nat))
; true
(display-bool (iszero? one-nat))
; false
(display-bool (equal-nat? one-nat one-nat))
; true
(display-bool (equal-nat? one-nat four-nat))
; false

;------------------------------------------
; definition of pairs

(define (pair x y)
  (lambda (m) (m x y)))

(define (fst p)
  (p true))

(define (snd p)
  (p false))

; test

(define p-nat (pair four-nat five-nat))
(display-nat (fst p-nat))
; 4
(display-nat (snd p-nat))
; 5
(define p-bool (pair true false))
(display-bool (fst p-bool))
; true
(display-bool (snd p-bool))
; false

;--------------------------------------
; definition of Y-combinator
(define (fix f)
  ((lambda (x) (lambda (y) (f (x x) y)))
   (lambda (x) (lambda (y) (f (x x) y)))))

(define fact
  (lambda (f n)
    (force (test (iszero? n) (delay one-nat) (delay (mul-nat n (f (pred-nat n))))))))

(define (factorial n) ((fix fact) n))

(display-nat (factorial five-nat))
; 120
