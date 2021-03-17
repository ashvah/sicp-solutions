#lang sicp

(define (random request)
  (define (random-update x)
    (remainder (+ (* 4 x) 3) 1024))
  (define random-init 7)
  (define (get-next last req)
    (cond ((eq? (car req) 'generate) (random-update last))
          ((eq? (car req) 'reset) (cadr req))
          (else (error "UNKNOWN REQUEST --RANDOM" (stream-car req)))))
  (define (iter last-rand req-stream)
    (if (stream-null? req-stream)
        the-empty-stream
        (let ((next (get-next last-rand (stream-car req-stream))))
          (cons-stream next
                       (iter next (stream-cdr req-stream))))))
  (cons-stream random-init
               (iter random-init request)))

(define (list-to-stream request-list)
  (define (iter l)
    (if (stream-null? l)
        the-empty-stream
        (cons-stream (car l)
                     (iter (cdr l)))))
  (iter request-list))


(define random-stream (random (list-to-stream '((generate) (generate) (reset 3) (generate) (reset 5) (generate)))))
(display-stream random-stream)
