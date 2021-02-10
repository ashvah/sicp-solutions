#lang sicp

(define (project-rational n)
  (make-scheme-number (numer n)))

(define (project-real n)
  (let ((r (rationalize (inexact->exact n))))
    (make-rat (numerator r) (denominator r))))

(define (project-complex n)
  (make-real (real-part n)))

(put 'project 'rational project-rational)
(put 'project 'real project-real)
(put 'project 'complex project-complex)

(define (project x)
  (let (proc (get 'project (type-tag x)))
    (if proc
        (apply proc (contents x))
        (error "No method for these types"
               (list 'project (type-tag x))))))

(define (drop n)
  (let ((project-raise (raise (project n))))
    (if (equ? project-raise n)
        (project n)
        n)))

(define (apply-generic op . args)
  (define (raise-to n arg)
    (define (iter current current-n)
      (if (= current-n n)
          current
          (iter (raise current) (inc n))))
    (iter arg (number-of-type (type-tag args))))
  (define (apply-generic-iter current-args)
    (let ((type-tags (map type-tag current-args)))
      (let ((proc (get op type-tags)))
        (if proc
            (drop (apply proc (map contents current-args)))
            (if (>= (length args) 2)
                (let ((number-list (map number-of-type type-tags)))
                  (let ((n (max number-list)))
                    (drop (apply-generic-iter (map (lambda (arg) (raise-to n arg)) current-args)))))
                (error "No method for these types"
                       (list op type-tags)))))))
  (apply-generic-iter args))
