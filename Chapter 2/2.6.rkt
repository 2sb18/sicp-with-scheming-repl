#lang planet neil/sicp


; (lambda (x) x) is the identity function. returning the passed number
; (lambda (f) (lambda (x) x)) is a function that does give a shit about
; what it's passed, it just returns the identity function
(define zero  (lambda (f) (lambda (x) x)))
(define one   (lambda (f) (lambda (x) (f x))))
(define two   (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; one returns (lambda (f) (lambda (x) (f x)))
; (one f) returns (lambda (x) (f x))
; ((one f) x) returns (f x)

(define (add-churches a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (get-number church-numeral)
  ((church-numeral inc) 0))

(define four (add-churches two two))

(define five (add-1 four))

(get-number (add-churches four five))










               







                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

