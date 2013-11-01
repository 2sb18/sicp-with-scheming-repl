#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

((compose square inc) 6)

(define (repeated procedure n)
  (if (= n 1)
      (lambda (x) (procedure x))
      (compose procedure (repeated procedure (- n 1)))))

((repeated square 2) 5)

((repeated square 3) 5)
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

