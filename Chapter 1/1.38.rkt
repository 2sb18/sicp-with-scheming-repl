#lang planet neil/sicp

(define (cont-frac n d k)
  (define (term i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (term (+ i 1))))))
  (term 1))

(define (denom i)
  (if (= 2 (remainder i 3))
      (* 2 (+ i 1) 1/3)
      1))

(define (e k)
  (+ 2 
     (cont-frac (lambda (i) 1.0) 
                (lambda (i) (if (= 2 (remainder i 3)) (* 2 (+ i 1) 1/3) 1)) 
                k)))
     
     
     


