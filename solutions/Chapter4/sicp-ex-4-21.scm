#lang sicp

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

; exactly Y-combinator

; a)
((lambda (n)
   ((lambda (f)
     (f f n))
   (lambda (fib k)
     (cond ((= k 0) 1)
           ((= k 1) 1)
           (else (+ (fib fib (- k 1)) (fib fib (- k 2))))))))
 10)

; or
(((lambda (f) ((lambda (x) (lambda (y) (f (x x) y)))
               (lambda (x) (lambda (y) (f (x x) y)))))
  (lambda (fib k)
    (cond ((= k 0) 1)
          ((= k 1) 1)
          (else (+ (fib (- k 1)) (fib (- k 2)))))))
 10)

; b)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (if (< n 0) (+ n 1) (- n 1)))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (if (< n 0) (+ n 1) (- n 1)))))))

(f -1001)
(f 12)
(f -8)
