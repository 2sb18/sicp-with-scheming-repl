; is it better to put the scan-out defines in the make-procedure
; procedure, or in the procedure-body procedure?

; what calls the make-procedure? only eval when an expression is found
; to be a lambda. 

; what calls procedure-body? our-apply

; we want to do the scan out in the make-procedure instead of in the
; procedure-body. this way, the defines are scanned out once, when the
; procedure is created. If they were in our-apply, they would be scanned
; out before every application of the procedure


#lang planet neil/sicp
(#%require "../eval-apply.rkt")
(#%require "../test-eval-apply.rkt")

(its '(lambda (x) 3))

(its-and-check '((lambda (x)
                   (* x (meow x))
                   (define (meow x)
                     (* x x)))
                 4)
               64)


