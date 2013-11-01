#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

; we've changed the constructors, and all we have to do for
; the program to work is change the selectors a bit.
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; selectors
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))


(define d (make-mobile (make-branch 3 5) (make-branch 2 2)))
(define b (make-mobile (make-branch 4 5) (make-branch 1 d)))
(define c (make-mobile (make-branch 6 2) (make-branch 7 1)))
(define a-unbalanced (make-mobile (make-branch 3 b) (make-branch 2 c)))
(define a-balanced (make-mobile (make-branch 3 b) (make-branch 12 c)))

(total-weight a-unbalanced)

(balanced? a-unbalanced)
(balanced? a-balanced)



       