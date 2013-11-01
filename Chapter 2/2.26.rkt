#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 '()))))))

(newline)

(cons x y)
(cons (cons 1 (cons 2 (cons 3 '()))) (cons 4 (cons 5 (cons 6 '()))))

(newline)

(list x y)
(cons (cons 1 (cons 2 (cons 3 '()))) (cons (cons 4 (cons 5 (cons 6 '()))) '()))
