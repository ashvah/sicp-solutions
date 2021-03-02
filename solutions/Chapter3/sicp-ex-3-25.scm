#lang sicp

(define (make-table)
  (define (assoc key records)
    (cond ((or (null? records) (not (pair? records))) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup-iter table key-list)
        (if (null? key-list)
            (cdr table)
            (let ((subtable (assoc (car key-list) (cdr table))))
              (if subtable
                  (lookup-iter subtable (cdr key-list))
                  false))))
      (lookup-iter local-table key-list))
    
    (define (insert! key-list value)
      (define (insert-iter table key-list)
        (if (null? key-list)
            (set-cdr! table value)
            (let ((subtable (assoc (car key-list) (cdr table))))
              (if subtable
                  (insert-iter subtable (cdr key-list))
                  (begin
                    (set-cdr! table (cons (cons (car key-list) nil) (cdr table)))
                    (insert-iter (cadr table) (cdr key-list)))))))
      (insert-iter local-table key-list)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define score (make-table))
(define get (score 'lookup-proc))
(define put (score 'insert-proc!))

(put '(math a) 93.42)
(put '(math b) 93.04)
(put '(math c) 98.12)
(put '(english d) 83.29)
(put '(english e) 80.92)
(put '(english c) 88.45)
(get '(math c))
; 98.12
(get '(english e))
; 80.92
(put '(english e) 100)
(get '(english e))
; 100
(put '(cs b 2021-3-2) 89)
(get '(cs b 2021-3-2 2))
; #f
(get '(math b 2021-3-2 2))
; #f
