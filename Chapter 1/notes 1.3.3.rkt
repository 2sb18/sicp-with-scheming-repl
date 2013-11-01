#lang planet neil/sicp

(define (search f neg-point pos-point)
  (let ((midpoint (* 0.5 (+ neg-point pos-point))))
    (if (< (abs (- pos-point neg-point)) 0.00001)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(search (lambda (x) (+ (* x x 2.0) (* 3.0 x) (* -2.0))) 0 -3)

; this figures out which is the neg-point and which is the pos-point
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0)) 
           (search f a b))
          ((and (< b-value 0) (> a-value 0))
           (search f b a))
          (else
           (error ("Values are not of opposite sign" a b))))))


(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0 2.0)



