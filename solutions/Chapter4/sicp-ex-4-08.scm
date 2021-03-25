#lang sicp

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let-vars exp)
  (if (pair? (cadr exp))
      (map car (cadr exp))
      (map car (caddr exp))))

(define (let-exps exp)
  (if (pair? (cadr exp))
      (map cadr (cadr exp))
      (map cadr (caddr exp))))

(define (let-body exp)
  (if (pair? (cadr exp))
      (cddr exp)
      (cdddr exp)))

(define (let-combination exp)
  (if (pair? (cadr exp))
      (cons 'call (cons (make-lambda (let-vars exp) (let-body exp)) (let-exps exp)))
      (list (make-lambda '()
            (list (list 'define (cadr exp) (make-lambda (let-vars exp) (let-body exp)))
                  (cons 'call (cons (cadr exp) (let-exps exp))))))))

(define eval-let
  (lambda (exp env) (eval (let-combination exp) env)))

(put 'eval 'let let-combination)

(let-combination '(let ((x 1) (y 2)) (+ x y)))
; (call (lambda (x y) (+ x y)) 1 2)
(let-combination '(let fib-iter ((a 1)
                                 (b 0)
                                 (count n))
                    (if (= count 0)
                        b
                        (fib-iter (+ a b) a (- count 1)))))

; ((lambda ()
;    (define fib-iter
;      (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
;    (call fib-iter 1 0 n)))
