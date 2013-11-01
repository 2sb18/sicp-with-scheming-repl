#lang planet neil/sicp

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accum-iter combiner null-value term a next b)
  (define (loop a result)
    (if (> a b)
        result
        (loop (next a) (combiner result (term a)))))
         
  (loop a null-value))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
#|
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
|#

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-iter term a next b)
  (accumulate * 1 term a next b))


(define (pi n)
  (define (factor x)
    (if (odd? x) 
        (/ (+ x 1.0) (+ x 2.0))
        (/ (+ x 2.0) (+ x 1.0))))
  (* 4 (product factor 1 inc n)))

(define (pi-iter n)
  (define (factor x)
    (if (odd? x) 
        (/ (+ x 1.0) (+ x 2.0))
        (/ (+ x 2.0) (+ x 1.0))))
  (* 4 (product-iter factor 1 inc n)))

