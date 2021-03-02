#lang sicp

(define (make-deque) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-item value) (cons '() (cons value '())))
(define (prior item) (car item))
(define (next item) (cddr item))
(define (value item) (cadr item))
(define (set-prior! item ptr) (set-car! item ptr))
(define (set-next! item ptr) (set-cdr! (cdr item) ptr))

(define (empty-deque? queue)
  (or (null? (front-ptr queue))
      (null? (rear-ptr queue))))

(define (front-deque queue)
  (if (empty-deque? queue)
      (error "FRONT called with an empty queue" queue)
      (value (front-ptr queue))))

(define (rear-deque queue)
  (if (empty-deque? queue)
      (error "FRONT called with an empty queue" queue)
      (value (rear-ptr queue))))

(define (front-insert-deque! queue value)
  (let ((new-pair (make-item value)))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-next! new-pair (front-ptr queue))
           (set-prior! (front-ptr queue) new-pair)
           (set-front-ptr! queue new-pair)
           queue))))

(define (rear-insert-deque! queue value)
  (let ((new-pair (make-item value)))
    (cond ((empty-deque? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-prior! new-pair (rear-ptr queue))
           (set-next! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (front-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (next (front-ptr queue)))
         (if (not (null? (front-ptr queue)))
             (set-prior! (front-ptr queue) nil))
         queue)))

(define (rear-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-rear-ptr! queue (prior (rear-ptr queue)))
         (if (not (null? (rear-ptr queue)))
             (set-next! (rear-ptr queue) nil))
         queue)))

(define (traverse-deque q f)
  (define (iter ptr)
    (if (not (null? ptr))
        (begin
          (f ptr)
          (iter (next ptr)))))
  (iter (front-ptr q)))

(define (print-deque q)
  (traverse-deque q (lambda (item) (display (value item)) (display "->")))
  (display "nil\n"))

(define q (make-deque))
(print-deque q)
; nil
(front-insert-deque! q 'a)
(print-deque q)
; a->nil
(front-insert-deque! q 'b)
(print-deque q)
; b->a->nil
(rear-insert-deque! q 'c)
(print-deque q)
; b->a->c->nil
(rear-delete-deque! q)
(print-deque q)
; b->a->nil
(rear-delete-deque! q)
(print-deque q)
; b->nil
(rear-insert-deque! q 'f)
(print-deque q)
; b->f->nil
(rear-deque q)
; f
(front-deque q)
; b
(front-delete-deque! q)
(print-deque q)
; f->nil
