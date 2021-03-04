; and-gate
; (0,1)
; (1,1) (set-signal! output 1)->segment
; (1,0) (set-signal! output 0)->segment
; FIFO:
;   (set-signal! output 1)
;   (set-signal! output 0)
;   answer is 0 (correct)
; LIFO:
;   (set-signal! output 0)
;   (set-signal! output 1)
;   answer is 1 (wrong)
