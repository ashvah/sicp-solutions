#lang sicp

; a)
(define (make-semaphore n)
  (let ((cnt 0)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex-cnt 'acquire)
             (if (< cnt n)
                 (begin
                   (set! cnt (inc cnt))
                   (mutex-cnt 'release))
                 (begin
                   (mutex-cnt 'release)
                   (the-semaphore 'acquire))))
            ((eq? m 'release)
             (mutex-cnt 'acquire)
             (set! cnt (- cnt 1))
             (mutex-cnt 'release))))
    the-semaphore))

; b)

(define (make-semaphore n)
  (let ((cnt 0))
    (define (test-and-set!)
      (without-interrupts
       (lambda ()
         (if (>= cnt n)
             true
             (begin (set! cnt (inc cnt))
                    false)))))
    (define (clear!)
      (without-interrupts
       (lambda ()
         (set! cnt (- cnt 1)))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-semaphore))
