#lang planet neil/sicp

(define (next test-divisor)
  (if (= test-divisor 2) 3
      (+ test-divisor 2)))

(define (nexty test-divisor)
  (+ test-divisor 1))

(define (smallest-divisor funct n)
  (define (find-divisor n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
     ((= (remainder n test-divisor) 0) test-divisor)
        (else (find-divisor n (funct test-divisor)))))
  (find-divisor n 2))

(define (prime? funct n)
  (= n (smallest-divisor funct n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes funct start)
  (if (prime? funct start) start
      (search-for-primes funct (+ start 1))))

(search-for-primes next 1E14)   ; 2268     57%
;(search-for-primes next 1E15)   ; 6692     55%
;(search-for-primes nexty 1E14)  ; 3930
;(search-for-primes nexty 1E15)  ; 12175    4.91

; only searching half the numbers gives almost a 2x gain. It's not quite 2x because
; it takes a bit to check what number we're on and skip over the even numbers


