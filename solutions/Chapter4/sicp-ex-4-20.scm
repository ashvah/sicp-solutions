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

(define (make-let var-exp body)
  (cons 'let (cons var-exp body)))

(define (letrec->let exp)
  (let ((vars (let-vars exp))
        (exps (let-exps exp))
        (body (let-body exp)))
    (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
              (append (map (lambda (x y) (list 'set! x y)) vars exps) body))))

(letrec->let '(letrec ((even?
                        (lambda (n)
                          (if (= n 0)
                              true
                                (odd? (- n 1)))))
                       (odd?
                        (lambda (n)
                          (if (= n 0)
                              false
                              (even? (- n 1))))))
                (if (even? x)
                    true
                    false)))


(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

(put 'eval 'letrec eval-letrec)
