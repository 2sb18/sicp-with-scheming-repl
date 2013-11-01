







#lang planet neil/sicp


(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
;  (display "stream-cdr happening\n")
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else
          (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car streams))
                        (apply stream-map (cons proc (map stream-cdr streams))))))


; this takes in a stream and outputs a stream
(define (stream-filter predicate stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((predicate (stream-car stream))
         (cons-stream (stream-car stream)
               (stream-filter predicate (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (add-streams . args)
  (apply stream-map (cons + args)))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (display-stream-partial stream start end)
  (display (stream-ref stream start))
  (newline)
  (if (not (eq? start end))
    (display-stream-partial stream (+ 1 start) end)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define zero-stream
  (cons-stream 0 zero-stream))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (scale-stream (stream-cdr s1) (stream-car s2)) 
                                                                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))


;Exercise 3.61.  Let S be a power series (exercise 3.59) whose constant term is 1. Suppose we want to find the power series 1/S, that is, the series X such that S · X = 1. Write S = 1 + SR where SR is the part of S after the constant term. Then we can solve for X as follows:

;In other words, X is the power series whose constant term is 1 and whose higher-order terms are given by the negative of SR times X. Use this idea to write a procedure invert-unit-series that computes 1/S for a power series S with constant term 1. You will need to use mul-series from exercise 3.60. 

(define (negate-stream s)
  (cons-stream (- (stream-car s)) (negate-stream (stream-cdr s))))

; the input is a unit-series, as in the constant term is 1
(define (invert-unit-series unit-series)
  (if (not (eq? (stream-car unit-series) 1))
    (error "input is not a unit-series -- INVERT-UNIT-SERIES" unit-series)
    (cons-stream 1 (negate-stream (mul-series (invert-unit-series unit-series) (stream-cdr unit-series))))))

(define streamy (integers-starting-from 1))

(define inverty (invert-unit-series streamy))

(define timey (mul-series streamy inverty))

(display-stream-partial timey 0 10)
