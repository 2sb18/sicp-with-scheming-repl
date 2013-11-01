#lang planet neil/sicp

(define (square n)
  (* n n))  

; this is one I came up with. It takes advantage of the fact
; that (a^b) mod m = ((a mod m)^b) mod m
(define (sexpmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (sexpmod (square (remainder base m)) (/ exp 2) m))
        (else
         (remainder (* base (sexpmod base (- exp 1) m)) m))))

(define (display-fermats n)
  (define (loop i)
    (if (= i (- n 1)) ((display "meow") (display "hat"))
        (display (sexpmod i n n))))
  (loop 0))

(define (fermat-test n)
  (define (test a)
    (if (= a 0) #t
        (if (= (sexpmod a n n) (remainder a n)) (test (- a 1))
            #f)))
  (test (- n 1)))

(fermat-test 3)   ; should be true, since 3 is prime
(fermat-test 60)  ; should be false, since 60 is not prime
(fermat-test 104729)  ; should be true, since this is prime

; all these should be true, since they're Carmichael numbers
(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)

(fermat-test 98765)

          

    