#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

; returns a function of the sent function
; repeated n times
(define (repeated procedure n)
  (if (= n 1)
      (lambda (x) (procedure x))
      (compose procedure (repeated procedure (- n 1)))))

(define dx 0.000001)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (root n r damps)
  (fixed-point ((repeated average-damp damps) (lambda (x) (/ n (expt x (- r 1))))) 20.0))

(define (iterative-improve good-enough-procedure
                           improve-guess-procedure)
  (lambda (x) 
    (define (try guess)
                (let ((next (improve-guess-procedure guess)))
                  (if (good-enough-procedure guess)
                      next
                      (try next))))
    (try x)))

(define (fixed-point-ii f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
                     (lambda (x) (f x))) first-guess))

(fixed-point (average-damp (lambda (x) (/ 2 x))) 1.0)
(fixed-point-ii (average-damp (lambda (x) (/ 2 x))) 1.0)

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt y)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) y)) 0.001))
                      (lambda (guess) (average guess (/ y guess)))) 1.0))

(sqrt 2)
    

    
               







                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

