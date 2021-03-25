#lang sicp

; while
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (while-sequence exp)
  (cddr exp))

(define (while-predicate exp)
  (cadr exp))

(define (while->rec exp)
  (list 'call (list 'lambda
                    '()
                    (list 'define
                          '(self)
                          (list 'if (while-predicate exp) (cons 'begin (append (while-sequence exp) (list '(self))))))
                    '(self))))

; test
(while->rec '(while (< i 10)
                    (set! result (+ result i))
                    (set! i (inc i))))

(define i 0)
(define result 0)
((lambda ()
   (define (self)
     (if (< i 10)
         (begin
           (set! result (+ result i))
           (set! i (inc i))
           (self))))
   (self)))
result

; for
(define (init-for-var exp)
  (map car (cadr exp)))

(define (init-for-val exp)
  (map cadr (cadr exp)))

(define (predicate-for exp)
  (caddr exp))

(define (proc-for exp)
  (cadddr exp))

(define (body-for exp)
  (cddddr exp))

(define (for->rec exp)
  (define (init vars values)
    (if (null? vars)
        '()
        (cons (list 'define (car vars) (car values)) (init (cdr vars) (cdr values)))))
  (list 'call
        (append (cons 'lambda
                      (cons '()
                            (cons (list 'define
                                        '(self)
                                        (list 'if
                                              (predicate-for exp)
                                              (cons 'begin
                                                    (append (body-for exp)
                                                            (list (proc-for exp)
                                                                  '(self))))))
                                  (init (init-for-var exp) (init-for-val exp)))))
                '((self)))))

(for->rec '(for ((j 0)
                 (result1 0))
             (< j 10)
             (set! j (inc j))
             (set! result1 (+ result1 j))))


((lambda ()
   (define (self)
     (if (< j 10) (begin (set! result1 (+ result1 j)) (set! j (inc j)) (self))))
   (define j 0)
   (define result1 0)
   (self)
   result1))
