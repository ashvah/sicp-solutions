#sicp

(define (union-set set1 set2)
  (append set1 set2))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((x1 (car set)))
        (cond ((= x x1) set)
              ((< x x1)
               (cons x set))
              ((> x x1)
               (cons x1 (adjoin-set x (cdr set))))))))

(adjoin-set 1 (list 1 2 3 4))
; (1 2 3 4)
(adjoin-set -5 (list 1 2 3 4))
; (-5 1 2 3 4)
(adjoin-set 2.5 (list 1 2 3 4))
; (1 2 2.5 3 4)
