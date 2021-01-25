#lang sicp

; in the applicative-order evaluation
; (square (expmod base (/ exp 2) m))
; we first evaluate k=(expmod base (/ exp 2) m), which costs f(exp/2) steps
; then we compute (square k), which costs one step
; so f(exp)=f(exp/2)+1=f(exp/4)+2=...=f(1)+log exp
; finally we get f(n)=\Theta(log n)
; 
; (*  (expmod base (/ exp 2) m)  (expmod base (/ exp 2) m))
; we first evaluate (expmod base (/ exp 2) m) twice, which costs 2f(exp/2) steps
; then we compute (* k k), which costs one step
; so f(exp)=2f(exp)+1=2(2f(exp/4)+1)+1=4f(exp/4)+2+1=...=2^{log exp}f(1)+2^{log exp-1}+..+1=2^{log exp}f(1)+2^{log exp}-1
; finnaly we get f(n)=\Theta(n)
