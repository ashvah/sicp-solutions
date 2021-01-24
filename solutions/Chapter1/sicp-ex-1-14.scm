#lang sicp
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)
(cc 11 5)
(+ (cc 11 (- 5 1)) (cc (- 11 50) 5))
(+ (cc 11 4) (cc -39 5))
(+ (+ (cc 11 (- 4 1)) (cc (- 11 25) 4)) 0)
(+ (+ (cc 11 3) (cc -14 4)) 0)
(+ (+ (+ (cc 11 (- 3 1)) (cc (- 11 10) 3)) 0) 0)
(+ (+ (+ (cc 11 2) (cc 1 3)) 0) 0)
(+ (+ (+ (+ (cc 11 (- 2 1)) (cc (- 11 5) 2)) (+ (cc 1 (- 3 1)) (cc (- 1 10) 3))) 0) 0)
(+ (+ (+ (+ (cc 11 1) (cc 6 2)) (+ (cc 1 2) (cc -9 3))) 0) 0)
(+ (+ (+ (+ (+ (cc 11 (- 1 1)) (cc (- 11 1) 1)) (+ (cc 6 (- 2 1)) (cc (- 6 5) 2))) (+ (+ (cc 1 (- 2 1)) (cc (- 1 5) 2)) 0)) 0) 0)
(+ (+ (+ (+ (+ (cc 11 0) (cc 10 1)) (+ (cc 6 1) (cc 1 2))) (+ (+ (cc 1 1) (cc -4 2)) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ (cc 10 (- 1 1)) (cc (- 10 1) 1))) (+ (+ (cc 6 (- 1 1)) (cc (- 6 1) 1)) (+ (cc 1 (- 2 1)) (cc (- 1 5) 2)))) (+ (+ (+ (cc 1 (- 1 1)) (cc (- 1 1) 1)) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ (cc 10 0) (cc 9 1))) (+ (+ (cc 6 0) (cc 5 1)) (+ (cc 1 1) (cc -4 2)))) (+ (+ (+ (cc 1 0) (cc 0 1)) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ (cc 9 (- 1 1)) (cc (- 9 1) 1)))) (+ (+ 0 (+ (cc 5 (- 1 1)) (cc (- 5 1) 1))) (+ (+ (cc 1 (- 1 1)) (cc (- 1 1) 1)) 0))) (+ (+ (+ 0 1) 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ (cc 9 0) (cc 8 1)))) (+ (+ 0 (+ (cc 5 0) (cc 4 1))) (+ (+ (cc 1 0) (cc 0 1)) 0))) (+ (+ 1 0) 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (cc 8 1)))) (+ (+ 0 (+ 0 (cc 4 1))) (+ (+ 0 1) 0))) (+ 1 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ (cc 8 (- 1 1)) (cc (- 8 1) 1))))) (+ (+ 0 (+ 0 (+ (cc 4 (- 1 1)) (cc (- 4 1) 1)))) (+ (+ 0 1) 0))) (+ 1 0)) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ (cc 8 0) (cc 7 1))))) (+ (+ 0 (+ 0 (+ (cc 4 0) (cc 3 1)))) (+ 1 0))) 1) 0) 0)
; ...
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 1)))) (+ (+ 0 (+ 0 (+ 0 1))) 1)) 1) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 1))) (+ (+ 0 (+ 0 1)) 1)) 1) 0) 0)
(+ (+ (+ (+ (+ 0 (+ 0 1)) (+ (+ 0 1) 1)) 1) 0) 0)
(+ (+ (+ (+ (+ 0 1) (+ 1 1)) 1) 0) 0)
(+ (+ (+ (+ 1 2) 1) 0) 0)
(+ (+ (+ 3 1) 0) 0)
(+ (+ 4 0) 0)
(+ 4 0)
4
; Time \Theta(n^5)
; Space \Theta(n)
