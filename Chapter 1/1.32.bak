#lang planet neil/sicp

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 10)

(define (pi n)
  (define (factor x)
    (if (odd? x) 
        (/ (+ x 1.0) (+ x 2.0))
        (/ (+ x 2.0) (+ x 1.0))))
  (* 4 (product factor 1 inc n)))

(pi 10)     ; 1 digit
(pi 100)    ; 2 digits
(pi 1000)   ; 3 digits
(pi 10000)  ; 4 digits
(pi 100000) ; 4 digits
(pi 1000000) ; 6 digits
; so approximately 1 digits more exact with an increase of 10x