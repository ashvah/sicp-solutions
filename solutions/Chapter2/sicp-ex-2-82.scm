#lang sicp

(define (apply-generic op . args)
  (define (transform proc-list args)
    (if (null? proc-list)
        nil
        (let ((p (car proc-list))
              (a (car args)))
          (cons (p a) (transform (cdr proc-list) (cdr args))))))
  (define (all-exist? proc-list)
    (if (null? all-exist)
        #t
        (and (all-exist? (cdr proc-list))
             (car proc-list))))
  (define (apply-generic-iter args)
    (let ((type-tags (map type-tag args)))
      (define (iter n args)
        (if (> n (length args))
            (error "No method for these types"(list op type-tags))
            (let ((type1 (car type-tags))
                  (rest (cdr type-tags)))
              (let ((proc-list (map (lambda (type) (get-coercion type type1)) rest)))
                (if (all-exist? proc-list)
                    (apply-generic-iter (cons (car args) (transform proc-list args)))
                    (iter (inc n) (append (cdr args) (car args))))))))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (>= (length args) 2)
              (iter 1 args)
              (error "No method for these types"
                     (list op type-tags)))))))
  (apply-generic-iter args))
