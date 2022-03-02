#lang sicp

;-----------------------------------
; tree
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;-----------------------------------
; Huffman tree
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (in s list)
    (if (null? list)
        false
        (if (eq? s (car list))
            true
            (in s (cdr list)))))
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
          '() 
          (error "bad symbol -- ENCODE-SYMBOL" symbol)) 
      (let ((lb (left-branch tree))
            (rb (right-branch tree)))
        (if (in symbol (symbols lb))
            (cons 0 (encode-symbol symbol lb))
            (cons 1 (encode-symbol symbol rb))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (if (= (length leafs) 1)
      (car leafs)
      (let ((l1 (car leafs))
            (l2 (cadr leafs))
            (rest (cddr leafs)))
        (successive-merge (adjoin-set (make-code-tree l1 l2) rest)))))

(define (traverse f list)
  (if (null? list)
      (newline)
      (begin
        (f (car list))
        (traverse f (cdr list)))))

;-----------------------------------
; test
(define sample-pairs '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1) ("\n" 5)))
(define sample-tree (generate-huffman-tree sample-pairs))
sample-tree
(define message '(get a job "\n" sha na na na na na na na na "\n" get a job "\n" sha na na na na na na na na "\n" wah yip yip yip yip yip yip yip yip yip "\n" sha boom))
(define bits (encode message sample-tree))
(length bits)
; 108
(display "Bits:\n")
(traverse (lambda (x) (display x)) bits)
(display "Message:\n")
(define message-generated (decode bits sample-tree))
(traverse (lambda (x) (display " ") (display x)) message-generated)
; Bits:
; 001110000010101000111111111111111110100111000001010100011111111111111111011001001010101010101010110100010011
; Message:
;  get a job 
;  sha na na na na na na na na 
;  get a job 
;  sha na na na na na na na na 
;  wah yip yip yip yip yip yip yip yip yip 
;  sha boom
