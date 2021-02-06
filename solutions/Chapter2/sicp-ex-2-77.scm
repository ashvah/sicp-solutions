#lang sicp

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define (magnitude z) (apply-generic 'magnitude z))

; z: ('complex ('rectangular (3 4)))
; apply-generic: 2 times
(magnitude z)
(apply-generic 'magnitude z)
((get 'magnitude (type-tag z)) (contents z))
((get 'magnitude 'complex) ('rectangular (3 4)))
(magnitude ('rectangular (3 4)))
(apply-generic 'magnitude ('rectangular (3 4)))
((get 'magnitude (type-tag ('rectangular (3 4)))) (contents ('rectangular (3 4))))
((get 'magnitude 'rectangular) (3 4))
