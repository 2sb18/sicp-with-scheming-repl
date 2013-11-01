#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

; pick 7 from the following

; (1 3 (5 7) 9)
(define l1 (list 1 3 (list 5 7) 9))
(car (cdaddr l1))

; ((7))
(define l2 (list (list 7)))
(caar l2)

; (1 (2 (3 (4 (5 (6 7))))))
(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr list3))))))

(define list4 (list 1 2 3))
(cddr list4)
