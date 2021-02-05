#lang sicp

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; a)
(define (get-record file employee)
  ((apply-generic 'record file) employee))

; b)
(define (get-salary file employee)
  ((apply-generic 'sarary file) employee))

; file should be something like (tag (emplyee-id other-info...))

; C)
(define (find-employee-record file-list employee)
  (if (null? file-list)
      (error "no result -- FIND-EMPLOYEE-RECORD" employee)
      (let ((file (car file-list))
            (rest (cdr file-list)))
        (append (get-salary file employee) (find-emplyee-record rest employee)))))

; d)
(define (install-pakage-xxx)
  (...))
