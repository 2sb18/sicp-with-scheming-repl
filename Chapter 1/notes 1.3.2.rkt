#lang planet neil/sicp

(define (f x y)
  (define (f-helper a b)
    (+ (* x a a)
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f-lambda x y)
  ((lambda (a b) (+ (* x a a)
                    (* y b)
                    (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x a a)
       (* y b)
       (* a b))))
    
    
(f 4 5)
(f-lambda 4 5)
(f-let 4 5)

(define x 3)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)






