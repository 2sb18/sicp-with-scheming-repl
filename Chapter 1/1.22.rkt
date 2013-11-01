#lang racket

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
     ((= (remainder n test-divisor) 0) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (time (smallest-divisor n))))

(define (search-for-primes start)
  (if (prime? start) start
      (search-for-primes (+ start 1))))

; 1E4  0     0
; 1E5  0     0
; 1E6  0     0 
; 1E7  0     1
; 1E8  16    3        3
; 1E9  16   11        3.7
; 1E10 31   35        3.2
; 1E11 156  155       4.4
; 1E12 390  396       2.6
; 1E13 1280 1273      3.2
; 1E14 4040 4037      3.17
; 1E15 12995 12762    3.16


