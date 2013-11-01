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

; this is O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; adds an object to a set, unless it already
; belongs to the set
; this is O(1)
(define (adjoin-set x set)
  (cons x set))

; intersection only includes objects that appear in
; both sets
; this is O(n^2) for two sets of size n
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; this is O(n) for two sets of size n
(define (union-set set1 set2)
  (cond ((null? set2) set1)
        (else (union-set (cons (car set2) set1) (cdr set2)))))


(pl (intersection-set '(1 2 3) '(1 3 5)))
(pl (union-set '(1 2 3 4) '(1 3 5 6)))
(pl (union-set '(1 2 3 4) '(1 2 3 4)))
(pl (union-set '(1) '(1)))












      












       