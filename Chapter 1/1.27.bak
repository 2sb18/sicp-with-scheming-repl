#lang planet neil/sicp

(define (square n)
  (* n n))
  
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))   

; this is one I came up with. It takes advantage of the fact
; that (a^b) mod m = ((a mod m)^b) mod m
(define (sexpmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (sexpmod (square (remainder base m)) (/ exp 2) m))
        (else
         (remainder (* base (sexpmod base (- exp 1) m)) m))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; first you get the exponential, then you find the remainder
(define (expmods base exp m)
  (remainder (fast-expt base exp) m))

    