


#lang planet neil/sicp 

(#%provide 
 stream-for-each
 display-stream
 display-line
 stream-ref
 stream-car
 stream-cdr
 stream-enumerate-interval
 stream-map
 stream-filter
 integers-starting-from
 add-streams
 mul-streams
 display-stream-partial
 scale-stream
 mul-series
 negate-stream
 invert-unit-series
 div-series
 integrate-series
 partial-sums 
 square
 prime?
 divides?
 constant-stream
 list-to-stream 
 )

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x)
  (* x x))


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

(define (list-to-stream lst)
  (if (null? lst)
    the-empty-stream
    (cons-stream (car lst) (list-to-stream (cdr lst)))))

(define (stream-cdr s)
;  (display "stream-cdr happening\n")
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else
          (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

; (define (stream-map proc stream)
;   (if (stream-null? stream)
;     the-empty-stream
;     (cons-stream (proc (stream-car stream))
;                  (stream-map proc (stream-cdr stream)))))

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

(define (constant-stream constant)
  (cons-stream constant
               (constant-stream constant)))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (scale-stream (stream-cdr s1) (stream-car s2)) 
                                                                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define (negate-stream s)
  (cons-stream (- (stream-car s)) (negate-stream (stream-cdr s))))

; the input is a unit-series, as in the constant term is 1
(define (invert-unit-series unit-series)
  (if (not (eq? (stream-car unit-series) 1))
    (error "input is not a unit-series -- INVERT-UNIT-SERIES" unit-series)
    (cons-stream 1 (negate-stream (mul-series (invert-unit-series unit-series) (stream-cdr unit-series))))))

(define (div-series num-series denom-series)
  (if (eq? 0 (stream-car denom-series))
    (error "denom-series has a zero constant term -- DIV-SERIES" denom-series)
    (mul-series (scale-stream num-series (stream-car denom-series))
                (invert-unit-series (scale-stream denom-series (/ 1 (stream-car denom-series)))))))

(define (integrate-series power-stream)
  (define (iter stream index)
   (if (stream-null? stream)
    'done
    (cons-stream (/ (stream-car stream) (+ 1 index))
                 (iter (stream-cdr stream) (+ 1 index)))))
  (iter power-stream 0))


(define (partial-sums s) (add-streams s (cons-stream 0 (partial-sums s))))
