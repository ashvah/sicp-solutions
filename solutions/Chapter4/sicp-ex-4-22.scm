#lang sicp

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

(define (analyze-let exp) (analyze (let-combination exp)))
