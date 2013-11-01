#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

(define (square x)
  (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))    ; putting stuff infront of answer
                    answer))))
  (iter items nil))

(define (square-list-switch items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things)))))) ; answer is a list!
  (iter items nil))

(define (square-list-steves items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (if (null? answer)
                  (list (square (car things)))
                  (append answer (list (square (car things))))))))
  (iter items nil))

(pl (square-list-steves (list 1 2 3 4 5 6)))
