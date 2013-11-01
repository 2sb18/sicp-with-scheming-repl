#lang planet neil/sicp

; print list, can be nested
; makes all the spacing correct!
(define (pl n)
  (define (in-loop n space?)
    (cond ((null? n) 0)
          ((not (pair? n)) (if space? (display " ")) (display n))
          ((list? (car n)) (out-loop (car n) #t) (in-loop (cdr n) #t))
          (else (if space? (display " ")) (display (car n)) (in-loop (cdr n) #t)))) 
  (define (out-loop n space?)
    (cond ((list? n) (if space? (display " ")) (display "(") (in-loop n #f) (display ")"))
          (else (in-loop n #f))))
  (out-loop n #f) (display "\n"))

(define (memq item seq)
  (cond ((null? seq) #f)
        ((eq? item (car seq)) seq)
        (else (memq item (cdr seq)))))

(define (equal? seq1 seq2)
  (cond ((and (null? seq1) (null? seq2)) #t)
        ((or (null? seq1) (null? seq2)) #f)
        ((not (eq? (car seq1) (car seq2))) #f)
        (else (equal? (cdr seq1) (cdr seq2)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
















      












       