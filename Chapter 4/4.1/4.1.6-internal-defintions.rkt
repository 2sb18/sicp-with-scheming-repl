#lang planet neil/sicp 

(#%require "../eval-apply.rkt")


(its 
  '(define (meow x)
     (let ((y 3))
       (* x y))))

(its '(meow 6))



