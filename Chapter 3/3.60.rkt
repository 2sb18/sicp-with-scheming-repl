







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


;Exercise 3.60.  With power series represented as streams of coefficients as in exercise 3.59, adding series is implemented by add-streams. Complete the definition of the following procedure for multiplying series:

; doesn't look like I can do it the way they want, let's see if I can just do it some way!

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define zero-stream
  (cons-stream 0 zero-stream))
; this is MY WAY!!!!
; looking on the internet, this is also the internet's way.
; bittersweet!
; this problem took me forever to solve, until I drew out diagrams of the streams, and lines
; between numbers in the streams showing what gets multiplied by what. That's when the pattern
; emerged.
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (scale-stream (stream-cdr s1) (stream-car s2)) 
                                                                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(display-stream-partial (mul-series (integers-starting-from 1) (integers-starting-from 4)) 0 50)

(define (negate-stream stream)
  (cons-stream (* -1 (stream-car stream))
               (negate-stream (stream-cdr stream))))

; input starts with the constant
; output starts with the first non-constant (which is the constant
; that goes along with x)
(define (integrate-series power-stream)
  (define (iter stream index)
   (if (stream-null? stream)
    'done
    (cons-stream (/ (stream-car stream) (+ 1 index))
                 (iter (stream-cdr stream) (+ 1 index)))))
  (iter power-stream 0))


; testing with sin^2 + cos^2 = 1
(define cosine-series
  (cons-stream 1 (negate-stream (integrate-series (cons-stream 0 (integrate-series cosine-series)))))) 
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define should-equal-constant-1
  (add-streams (mul-series cosine-series cosine-series)
               (mul-series sine-series sine-series)))

(display-stream-partial should-equal-constant-1 0 10)

; WORKS!
