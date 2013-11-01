#lang planet neil/sicp

(define (number-of-factors factor number)
  (define (loop number factors)
    (if (= (remainder number factor) 0)
        (loop (/ number factor) (+ factors 1))
        factors))
  (loop number 0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (number-of-factors 2 z))

(define (cdr z)
  (number-of-factors 3 z))




               







                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

