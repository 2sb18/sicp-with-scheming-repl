#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (simpson-integral f a b n)
  (define (multiplier k)
    (cond ((or (= k 0) (= k n)) 1.0)
          ((even? k) 2.0)
          (else 4.0)))
  (define (sum k)
    (if (> k n)
        0
        (+ (* (multiplier k) (f (+ a (* k (- b a) (/ 1.0 n)))))
           (sum (+ k 1)))))
  (* (- b a) (/ 1.0 3.0 n) (sum 0)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(simpson-integral cube 0 1 100)     ; better results
(simpson-integral cube 0 1 1000)
