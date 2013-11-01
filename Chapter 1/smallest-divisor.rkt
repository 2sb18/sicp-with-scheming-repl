#lang racket

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
     ((= (remainder n test-divisor) 0) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))
