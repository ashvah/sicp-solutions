#lang sicp

(define (number-of-type type)
  (define type-list '(scheme-number rational real complex))
  (define (search types result)
    (if (null? type-list)
        (error "Cannot find these types" type)
        (let ((t1 (car types))
              (rest (cdr types)))
          (cond ((eq? t1 type) result)
                (else (search rest (inc result)))))))
  (search type-list 0))

(define (apply-generic op . args)
  (define (raise-to n arg)
    (define (iter current current-n)
      (if (= current-n n)
          current
          (iter (raise current) (inc n))))
    (iter arg (number-of-type (type-tag args))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (>= (length args) 2)
              (let ((number-list (map number-of-type type-tags)))
                (let ((n (max number-list)))
                  (apply-generic op (map (lambda (arg) (raise-to n arg)) args))))
              (error "No method for these types"
                     (list op type-tags)))))))
