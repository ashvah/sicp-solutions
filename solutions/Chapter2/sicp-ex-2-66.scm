#lang sicp

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry set-of-records)) true)
        ((< given-key (entry set-of-records)) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))
