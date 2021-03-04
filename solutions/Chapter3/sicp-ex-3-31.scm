#lang sicp

(probe 'sum sum)

(probe 'carry carry)


(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)
; carry 11  New-value = 1
