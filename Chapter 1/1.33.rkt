#lang planet neil/sicp

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

; this is one I came up with. It takes advantage of the fact
; that (a^b) mod m = ((a mod m)^b) mod m
(define (sexpmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (sexpmod (square (remainder base m)) (/ exp 2) m))
        (else
         (remainder (* base (sexpmod base (- exp 1) m)) m))))

; didn't implement the optimization shortcut on the sexpmod
; procedure
(define (prime? n)
  (define (test a)
    (cond ((= a 0) #t)
          ((= (sexpmod a (- n 1) n) 0) #f)
          ((= (sexpmod a (- n 1) n) 1) (test (- a 1)))
          (else #f)))
  (test (- n 1)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate combiner null-value filter term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value filter term (next a) next b))))


(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(sum-of-squares-of-primes 1 10)

(define (product-of-relative-primes n)
  (define (gcd-filter i)
    (if (= (gcd i n) 1)
        #t
        #f))
  (filtered-accumulate * 1 gcd-filter identity 1 inc (- n 1)))

(product-of-relative-primes 10)






