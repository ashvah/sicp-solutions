#lang sicp

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left-val (eval (first-operand exps) env)))
        (cons left-val
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right-val (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              right-val))))
