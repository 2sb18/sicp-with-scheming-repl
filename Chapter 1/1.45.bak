#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

((compose square inc) 6)

; returns a function of the sent function
; repeated n times
(define (repeated procedure n)
  (if (= n 1)
      (lambda (x) (procedure x))
      (compose procedure (repeated procedure (- n 1)))))

(define dx 0.000001)

(define (smooth f)
  (lambda (x) (/
               (+ (f (- x dx)) (f x) (f (+ x dx)))
               3)))

(define (n-smooth f n)
  (repeated (smooth f) n))

                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

