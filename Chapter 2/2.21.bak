#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

(define (same-parity p . l)
  (define (loop l)
    (if (null? l)
        nil
        (if (or (and (even? p) (even? (car l))) (and (odd? p) (odd? (car l))))
            (cons (car l) (loop (cdr l)))
            (loop (cdr l)))))
  (append (list p) (loop l)))


; procedure is a function which takes in an element of the list and produces an
; output. items is a list
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) 
            (map proc (cdr items)))))
  
  

(pl (same-parity 1 2 3 4 5 6 7))

(pl (same-parity 2 3 4 5 6 7))