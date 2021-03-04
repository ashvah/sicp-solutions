#lang sicp

;------------------------------------------------------------
; queue
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

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
;------------------------------------------------------------
; agenda
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
;------------------------------------------------------------
; wire
(define the-agenda (make-agenda))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))
;------------------------------------------------------------
; or and inverter gate
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (and-gate a1 a2 output)
  (define (logical-and a1 a2)
    (cond ((and (= a1 1) (= a2 1)) 1)
          (else 0)))
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (logical-or a1 a2)
    (cond ((or (= a1 1) (= a2 1)) 1)
          (else 0)))
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (inverter input output)
  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
;------------------------------------------------------------
; adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder A B S C)
  (define (iter A B S c)
    (if (null? A)
        (set-signal! c 0)
        (begin
          (let ((c-in (make-wire)))
            (iter (cdr A) (cdr B) (cdr S) c-in)
            (full-adder (car A) (car B) c-in (car S) c)))))
  (iter (cdr A) (cdr B) (cdr S) (cadr C)))
;------------------------------------------------------------
; n-bit data bus
(define (data-bus label n)
  (let ((bits '()))
    (define (iter n)
      (if (= n 0)
          'done
          (begin
            (set! bits (cons (make-wire) bits))
            (iter (- n 1)))))
    (iter n)
    (cons label bits)))

(define (set-data! db value)
  (define (iter list value)
    (if (null? list)
        'done
        (begin
          (set-signal! (car list) (car value))
          (iter (cdr list) (cdr value)))))
  (iter (cdr db) value))

(define (output-data db)
  (define (print-data list)
    (if (null? list)
        (display "\n")
        (begin
          (display (get-signal (car list)))
          (print-data (cdr list)))))
  (display (car db))
  (display ": ")
  (print-data (cdr db)))
;------------------------------------------------------------
; test
(define A (data-bus 'input-1 8))
(define B (data-bus 'input-2 8))
(define S (data-bus 'sum 8))
(define C (data-bus 'carry 1))
(ripple-carry-adder A B S C)

; test-1
(set-data! A '(0 0 0 0 0 0 0 1))
(set-data! B '(0 0 0 0 0 0 1 1))
(propagate)
(output-data A)
(output-data B)
(output-data S)
(output-data C)
; 1+11=100
; carry=0

; test-2
(set-data! A '(0 0 0 0 1 0 0 1))
(set-data! B '(0 1 0 1 1 0 1 1))
(propagate)
(output-data A)
(output-data B)
(output-data S)
(output-data C)
; 1001+1011011=01100100
; carry=0

; test-3
(set-data! A '(1 1 1 1 1 1 1 1))
(set-data! B '(1 1 0 1 1 0 1 1))
(propagate)
(output-data A)
(output-data B)
(output-data S)
(output-data C)
; 11111111+1011011=11011010
; carry=1

; test-4
(set-data! A '(0 0 1 1 1 1 1 1))
(set-data! B '(1 0 0 1 1 0 1 1))
(propagate)
(output-data A)
(output-data B)
(output-data S)
(output-data C)
; 111111+1001011=11011010
; carry=0
