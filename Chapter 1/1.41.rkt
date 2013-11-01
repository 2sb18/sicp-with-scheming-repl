#lang planet neil/sicp

(define (double one-arg-procedure)
  (lambda (x) (one-arg-procedure (one-arg-procedure x))))

(((double (double double)) inc) 5)

; do inc 16 times. answer will be 5 + 16 = 21
(((double (double double)) inc) 5)
(((double (lambda (x) (double (double x)))) inc) 5)

; 2 doubles
(double double)

; 4 doubles
(double (double double))

; 2 to the 4 doubles is 16.

(define (super-doub x)
  (((double (double double)) (lambda (x) (* x x))) x))
    
  