#lang planet neil/sicp

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

(define (fringe x)
  (cond ((not (pair? x)) (list x))
        ((null? (cdr x)) (fringe (car x)))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
        

(fringe (list x x))
       