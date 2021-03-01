#lang sicp

(define (loop? x)
  (let ((list-visited '()))
    (define (iter first second)
      (if (or (not (pair? second)) (not (pair? (cdr second))))
          #f
          (let ((next-f (cdr first))
                (next-s (cdr (cdr second))))
            (if (eq? next-f next-s)
                #t
                (iter next-f next-s)))))
    (iter x x)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(loop? '(a b c))
; #f
(loop? (make-cycle (list 'a 'b 'c)))
; #t
(loop? '(a b c d))
; #f
(loop? (make-cycle (list 'a 'b 'c 'd)))
; #t
