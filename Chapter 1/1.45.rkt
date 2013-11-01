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

(define (test n damps)
  (display n)
  (newline)
  (root 2 n damps))

(test 3 1)
(test 4 2)
(test 5 2)
(test 6 2)
(test 7 2)
(test 8 3)
(test 9 3)
(test 10 3)
(test 11 3)
(test 12 3)
(test 13 3)
(test 14 3)
(test 15 3)
(test 16 4)
(test 31 4)
(test 32 5)

; number of average-damps needed is (floor (/ (log nth-root) (log 2)))







                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

