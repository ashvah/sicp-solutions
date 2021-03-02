#lang sicp

(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)))
    dispatch))

(define (front-ptr q)
  (q 'front-ptr))

(define (rear-ptr q)
  (q 'rear-ptr))

(define (set-front-ptr! q item)
  ((q 'set-front-ptr!) item))

(define (set-rear-ptr! q item)
  ((q 'set-rear-ptr!) item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue q)
  (define (traverse front)
    (if (null? front)
        (display "nil\n")
        (begin
          (display (car front))
          (display "->")
          (traverse (cdr front)))))
  (traverse (front-ptr q)))

(define q1 (make-queue))

; q1-->(*)(*)
;       |  |
;       v  v
;      nil nil

(insert-queue! q1 'a)

; q1-->(*)(*)
;       |  |
;       |  |
;       +--+--> (*)(\)
;                | 
;                v 
;                a
; ((a) a)

(print-queue q1)
; a->nil

(insert-queue! q1 'b)

; q1-->(*)(*)----------+
;       |              |
;       |              |
;       +-----> (*)(*)-+->(*)(\)
;                |         |
;                v         v
;                a         b
; ((a b) b)

(print-queue q1)
; a->b->nil

(delete-queue! q1)

; q1-->(*)(*)----------+
;       |              |
;       +--------------+
;                      |
;               (*)(*)-+->(*)(\)
;                |         |
;                v         v
;                a         b
; ((b) b)

(print-queue q1)
; b->nil

(delete-queue! q1)

; q1-->(\)(*)----------+
;                      |
;                      |
;                      |
;               (*)(*)-+->(*)(\)
;                |         |
;                v         v
;                a         b
; (() b)

(print-queue q1)
; nil
