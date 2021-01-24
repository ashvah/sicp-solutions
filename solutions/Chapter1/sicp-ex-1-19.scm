#lang sicp
; T_{p,q}(a,b)->(bq+aq+ap,bp+aq)
; T^2_{p,q}(a,b)->T_{p,q}(bq+aq+ap,bp+aq)
;              ->((bp+aq)q+(bq+aq+ap)q+(bq+aq+ap)p,(bp+aq)p+(bq+aq+ap)q)
;              ->(bpq+aq^2+bq^2+aq^2+apq+bpq+apq+ap^2,bp^2+apq+bq^2+aq^2+apq)
;              ->(b(2pq+q^2)+a(2pq+q^2)+a(p^2+q^2),b(p^2+q^2)+a(2pq+q^2))
; T^2_{p,q}(a,b)=T_{p^2+q^2,2pq+q^2}(a,b)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (square x) (* x x))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p'
                   (+ (* 2 p q) (square q)) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
