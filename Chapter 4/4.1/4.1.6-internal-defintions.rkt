#lang planet neil/sicp 

(#%require "../eval-apply.rkt")

(its 
  '(define (f x)
     (define (3? n)
       true)
     ; (if (= n 3)
     ;   true
     ;   false))
     ;  even? is defined in terms of odd?
     ; (define (even? n)
     ;   (if (= n 0)
     ;     true
     ;     (odd? (- n 1))))
     ; ; odd? is defined in terms of even?
     ; (define (odd? n)
     ;   (if (= n 0)
     ;     false
     ;     (even? (- n 1))))
     ; (if (even? x)
     ;   true
     ;   false)))
     (if (3? x)
       true
       false)))

(its 'f)

; (its '(f 2))
