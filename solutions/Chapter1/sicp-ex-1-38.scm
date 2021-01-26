#lang sicp

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (compute-e)
  (+ 2
     (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((= 2 (remainder i 3)) (* 2 (/ (+ i 1) 3)))
                     (else 1.0)))
             10)))

(compute-e)
