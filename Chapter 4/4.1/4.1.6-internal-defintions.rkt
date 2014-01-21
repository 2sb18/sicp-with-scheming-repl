#lang planet neil/sicp 

(#%require "../eval-apply.rkt")

(its 'the-global-environment)

(its 
  '(define (f x)
     (define (even? n)
       (if (= n 0)
         true
         (odd? (- n 1))))
     (define (odd? n)
       (if (= n 0)
         false
         (even? (- n 1))))
     (even? x)))

; this doesn't work cause the list references
; itself
; (its 'f)

(its 'the-global-environment)

#0=(
    (
     (f procedure (x) 
        (
         (define (even? n) 
           (if (= n 0) true (odd? (- n 1)))) 
         (define (odd? n) 
           (if (= n 0) false (even? (- n 1)))) 
         (even? x)) #0#) 
     (false . #f) 
     (true . #t) 
     (null? primitive #<procedure:null?>) 
     (display primitive #<procedure:mdisplay>) 
     (= primitive #<procedure:=>) 
     (* primitive #<procedure:*>) 
     (- primitive #<procedure:->) 
     (+ primitive #<procedure:+>) 
     (cons primitive #<procedure:mcons>) 
     (cdr primitive #<procedure:mcdr>) 
     (car primitive #<procedure:mcar>)
     ) 
    ()
    )
