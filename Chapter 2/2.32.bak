#lang planet neil/sicp

; print list, can be nested
(define (pl n)
  (define (in-loop n)
    (cond ((null? n) 0)
          ((not (pair? n)) (display " ") (display n))
          ((list? (car n)) (pl (car n)) (in-loop (cdr n)))
          (else (display " ") (display (car n)) (in-loop (cdr n))))) 
  (cond ((list? n) (display "(") (in-loop n) (display ")"))
        (else (in-loop n))))
    

(define (square-list1 items)
  (if (null? items)
      nil
      (cons ((lambda (x) (* x x)) (car items))
            (square-list1 (cdr items)))))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define  (square-tree tree)
  (define (square x) (* x x))
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define meow (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(pl (tree-map (lambda (x) (* x x)) meow))
(newline)
(pl (square-tree meow))






       