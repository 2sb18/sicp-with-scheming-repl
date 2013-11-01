#lang planet neil/sicp

(define one-through-four (list 1 2 3 4))

(define (print-list n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(print-list (last-pair (list 23 72 149 34)))

