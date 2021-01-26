#lang sicp

(define (square x) (* x x))

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (tan-cf x k)
     (cont-frac (lambda (i)
                  (cond ((= i 1) x)
                        (else (- (square x)))))
                (lambda (i) (- (* 2 i) 1))
                k))

(tan-cf (/ 3.141592653 4) 10)
; tan(pi/4)=1

(square (tan-cf (/ 3.141692653 3) 10))
; tan(pi/3)^2=3

(square (tan-cf (/ 3.141692653 6) 10))
; tan(pi/6)^2=1/3
