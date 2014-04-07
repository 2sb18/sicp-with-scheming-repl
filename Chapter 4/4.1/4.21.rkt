;Exercise 4.21.  Amazingly, Louis's intuition in exercise 4.20 is correct. It is indeed possible to specify recursive procedures without using letrec (or even define), although the method for accomplishing this is much more subtle than Louis imagined. The following expression computes 10 factorial by applying a recursive factorial procedure:27

#lang planet neil/sicp 

((lambda (n)
   ((lambda (fact)   ; the argument must be a function that takes two arguments.
      (fact fact n)) ; function is executed with itself as one input and n as the other
    (lambda (ft k) ; this is the function we send to the top function
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 10)

; n = 10

((lambda (fact)
   (fact fact 10))
 (lambda (ft k)            ; this is a prodcedure with two arguments
   (if (= k 1)
     1
     (* k (ft ft (- k 1))))))



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; 
((lambda (f)
   (f f 10))
 (lambda (fib k)
   (cond ((= k 0) 0)
         ((= k 1) 1)
         (else (+ (fib fib (- k 1))
                  (fib fib (- k 2)))))))

; cool, that works!


;Fill in the missing expressions to complete an alternative definition of f, which uses neither internal definitions nor letrec:

(define (f x)
  (define (even? n)
    (if (= n 0)
      true
      (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
      false
      (even? (- n 1))))
  (even? x))

(define (even x)
  ((lambda (run-and-pass pass)
     (run-and-pass run-and-pass pass x))
   ; the even procedure
   (lambda (ev? od? n)
     (if (= n 0) 
       true 
       (od? ev? od? (- n 1))))
   ; the odd procedure
   (lambda (ev? od? n)
     (if (= n 0) 
       false 
       (ev? ev? od? (- n 1))))))

; the functions get brought along for the right so that they never have to be named
