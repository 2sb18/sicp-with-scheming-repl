#lang planet neil/sicp

(define one-through-four (list 1 2 3 4))

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (reverse x)
  (if (null? (cdr x))
      x
      (append (reverse (cdr x)) (list (car x)))))


(pl (reverse (list 1 4 9 16 25)))