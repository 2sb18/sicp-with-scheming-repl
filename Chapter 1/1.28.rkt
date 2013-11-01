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

; df is for display-fermats, which isn't a great name!
(define (df n)
  (define (display-and-call i)
    (display (sexpmod i n n))
    (display " ")
    (loop (+ i 1)))
  (define (loop i)
    (if (= i n) (values)
        (display-and-call i)))
  (loop 0))

; didn't implement the optimization shortcut on the sexpmod
; procedure
(define (mb-test n)
  (define (test a)
    (cond ((= a 0) #t)
          ((= (sexpmod a (- n 1) n) 0) #f)
          ((= (sexpmod a (- n 1) n) 1) (test (- a 1)))
          (else #f)))
  (test (- n 1)))

(define (fermat-test n)
  (define (test a)
    (if (= a 0) #t
        (if (= (sexpmod a n n) (remainder a n)) (test (- a 1))
            #f)))
  (test (- n 1)))

(mb-test 3)   ; should be true, since 3 is prime
(mb-test 60)  ; should be false, since 60 is not prime
(mb-test 104729)  ; should be true, since this is prime

; all these should be false, since they're Carmichael numbers
; and we're using the miller-rabin test
(mb-test 561)
(mb-test 1105)
(mb-test 1729)
(mb-test 2465)
(mb-test 2821)
(mb-test 6601)

(mb-test 98765)

          

    