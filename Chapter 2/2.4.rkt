#lang planet neil/sicp

; returns a function that operates on x and y
(define (cons x y)
  (lambda (m) (m x y)))

(define (first x y)
  x)

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))








    

    
               







                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

