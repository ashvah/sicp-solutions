#lang sicp

(define (loop? x)
  (let ((list-visited '()))
    (define (iter rest)
      (if (not (pair? rest))
          #f
          (if (memq rest list-visited)
              #t
              (begin
                (set! list-visited (cons rest list-visited))
                (iter (cdr rest))))))
    (iter x)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(loop? '(a b c))
; #f
(loop? z)
; #t
