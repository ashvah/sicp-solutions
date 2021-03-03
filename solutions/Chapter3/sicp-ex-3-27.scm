#lang sicp

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; -->global: memo-fib
;               |                   ^
;               v                   |
;              (x)(*) -->E1: (f: lambda(x)...
;             (code)          table: empty-table)

(memo-fib 100)

; -->global: memo-fib
;               |                   ^
;               v                   |
;              (x)(*) -->E1: (f: lambda(x)...
;             (code)          table: empty-table)
;                                   ^ 
;                                   |
;               -->E2(x:100)--------+
;               -->E3(x:99)---------+
;               -->E4(x:98)---------+
;               ...                 |
;               -->E101(x:1)--------+
;               -->E102(x:0)--------+

; -->global: memo-fib
;               |                   ^
;               v                   |
;              (x)(*) -->E1: (f: lambda(x)...
;             (code)          table: '(1 1 2 3 ... 83621143489848422977))
;                                   ^ 
;                                   |
;               -->E2(x:100)--------+
;               -->E3(x:99)---------+
;               -->E4(x:98)---------+  (+ 51680708854858323072 83621143489848422977) insert...

; -->global: memo-fib
;               |                   ^
;               v                   |
;              (x)(*) -->E1: (f: lambda(x)...
;             (code)          table: '(1 1 2 3 ... 354224848179261915075))

; to compute f(n), just need to search for f(n-1) and f(n-2) in the table then add them together.
; T=O(n)

; ((memoize fib) n) will only record f(n), f(0) f(1)...f(n-1) will not be inserted into table.
; So there is no improvement
