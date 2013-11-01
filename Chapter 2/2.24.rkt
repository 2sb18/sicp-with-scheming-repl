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

(define l (list 1 (list 2 (list 3 4))))
