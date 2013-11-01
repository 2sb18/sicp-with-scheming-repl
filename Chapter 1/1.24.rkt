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

(define (big-crappy-random n)
  (floor (* (random 1000000000) (- n 1) 1/1000000000)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (big-crappy-random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (display " "))

(define (search-for-primes start)
  (if (timed-prime-test start) start
      (search-for-primes (+ start 1))))

(search-for-primes 1000000000)
(search-for-primes 10000000000)
(search-for-primes 100000000000)
(search-for-primes 1000000000000)
(search-for-primes 10000000000000)
(search-for-primes 100000000000000)






