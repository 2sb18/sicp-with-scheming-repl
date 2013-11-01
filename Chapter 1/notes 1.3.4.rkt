#lang planet neil/sicp


(define tolerance 0.00000001)

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
  (lambda (x) (* 0.5 (+ x (f x)))))

((average-damp (lambda (x) (* x x))) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
               1.0))

(define (root x r)
  (fixed-point (average-damp (lambda (y) (/ x (expt y (- r 1)))))
               1.0))

(define (compose second-func first-func)
  (lambda (x) (second-func (first-func x))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ 
               (- (g (+ x dx)) (g x))
               dx)))

(define (cube x) (* x x x))
(define (square x) (* x x))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) 
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


     
     
     


