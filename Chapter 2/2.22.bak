#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

; procedure is a function which takes in an element of the list and produces an
; output. items is a list
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) 
            (map proc (cdr items)))))

(define (square-list1 items)
  (if (null? items)
      nil
      (cons ((lambda (x) (* x x)) (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(pl (square-list1 (list 1 2 3 4)))
(pl (square-list2 (list 1 2 3 4)))
