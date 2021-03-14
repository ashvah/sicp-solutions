#lang sicp

; It need to compute (sqrt-stream x) many times
; If we don't use memo-proc, their efficiency are exactly the same
; Because we need to recalculate every terms of guesses
