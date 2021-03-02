#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup table key-1 key-2)
  ((table 'lookup-proc) key-1 key-2))

(define (insert! table key-1 key-2 item)
  ((table 'insert-proc!) key-1 key-2 item))

(define t (make-table (lambda (x y)
                        (if (and (number? x) (number? y))
                            (< (abs (- x y)) 0.2)
                            (equal? x y)))))
(insert! t 'math 1 93.42)
(insert! t 'math 2 93.04)
(insert! t 'math 3 98.12)
(insert! t 'english 4 83.29)
(insert! t 'english 5 80.92)
(insert! t 'english 6 88.45)
(lookup t 'math 4.1)
(lookup t 'english 5.9)
