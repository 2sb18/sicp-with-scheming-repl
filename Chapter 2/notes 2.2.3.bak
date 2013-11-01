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
  (out-loop n #f))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

; what I think is a clear explanation:
; 1. subsets gets all the subsets of a set
; 2. we can get all the subsets of a set X by appending two sets:
;       a) the subsets of (cdr X)
;       b) taking a) and adding (car X) to each element
; this is recursive


(pl (subsets (list 1 2 3 4 5 6 7)))









       