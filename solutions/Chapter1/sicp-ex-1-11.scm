#lang sicp
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3)))))))

(define (f-iter n)
  (define (f-iterative a b c n)
  (cond ((= n 2) a)
        (else (f-iterative (+ a (* 2 b) (* 3 c)) a b (- n 1)))))
  (if (< n 3)
      n
      (f-iterative 2 1 0 n)))

(f-recursive 10)
(f-iter 1000)
