#lang sicp

; exactly no
; a^n may be a very long number
; with the length of number growing, we cannot ignore the time spent in multiplying two large numbers
; it will cost a lot of time to compute a^n and (remainder a^n n)
; but expmod limits the length of numbers in 0~n-1
; it's faster in dealing with a very large number
