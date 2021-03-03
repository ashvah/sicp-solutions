#lang sicp

(define (make-tree left right entry)
  (list left right entry))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (get-entry tree)
  (caddr tree))

(define (get-key tree)
  (caaddr tree))

(define (get-value entry)
  (cdr entry))

(define (set-value! entry new-value)
  (set-cdr! entry new-value))

(define (set-left! tree ptr)
  (set-car! tree ptr))

(define (set-right! tree ptr)
  (set-car! (cdr tree) ptr))

(define (make-table)
  (define (less-than? a b)
    (cond ((eq? a '*) (< '0 b))
          ((eq? b '*) (< a '0))
          (else (< a b))))
  
  (define (assoc key records)
    (cond ((or (null? records) (not (pair? records))) false)
          ((equal? key (get-key records)) (get-entry records))
          ((less-than? key (get-key records)) (assoc key (left-branch records)))
          (else (assoc key (right-branch records)))))

  (define (assoc-insert key old records)
    (cond ((or (null? records)) old)
          ((equal? key (get-key records)) (get-entry records))
          ((less-than? key (get-key records)) (assoc-insert key records (left-branch records)))
          (else (assoc-insert key records (right-branch records)))))
  
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup-iter table key-list)
        (if (null? key-list)
            (get-value (assoc '* (get-value table)))
            (let ((subtable (assoc (car key-list) (get-value table))))
              (if subtable
                  (lookup-iter subtable (cdr key-list))
                  false))))
      (lookup-iter local-table key-list))
    
    (define (insert! key-list value)
      (define (insert-iter table key-list)
        (if (null? key-list)
            (set-value! table (make-tree '() '() (cons '* value)))
            (let ((subtable (assoc-insert (car key-list) '() (get-value table))))
              (cond ((null? subtable) (set-cdr! table (make-tree '() '() (list (car key-list))))
                                      (insert-iter (get-entry (get-value table)) (cdr key-list)))
                    ((equal? (car subtable) (car key-list)) (insert-iter subtable (cdr key-list)))
                    ((less-than? (car key-list) (get-key subtable)) (set-left! subtable (make-tree '() '() (list (car key-list))))
                                                           (insert-iter (get-entry (left-branch subtable)) (cdr key-list)))
                    (else (set-right! subtable (make-tree '() '() (list (car key-list))))
                          (insert-iter (get-entry (right-branch subtable)) (cdr key-list)))))))
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

(put '(1 1) 93.42)
(put '(1 2) 93.04)
(put '(2 3) 98.12)
(put '(5 3) 83.29)
(get '(2 3))
(get '(1 3 4))
(put '(1 2 3 4 5 6 7 8) 0)
(get '(1 2 3 4 5 6 7 8))
