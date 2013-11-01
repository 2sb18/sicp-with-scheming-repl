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

(define (reverse x)
  (if (null? (cdr x))
      x
      (append (reverse (cdr x)) (list (car x)))))

(define (deep-reverse x)
  (if (not (pair? x))
      x
      (if (null? (cdr x))
          (list (deep-reverse (car x)))
          (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))))
      
(define x (list (list 1 2) (list 3 4)))

x

(reverse x)

(deep-reverse x)