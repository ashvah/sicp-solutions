#lang sicp
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4
                  5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
(define (sumOfLargestTwoSquared x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
        ((and (< z y) (< z x)) (+ (* y y) (* x x)))))
