
; wires can be set to a constant
; if constant-value is '() then wire's value can change
; 
(define (make-wire constant-value)
  (lambda (op)
    (cond ((eq? op 'set-value) (lambda (value) (set! constant-value value)))
          ((eq? op 'get-value) (lambda () constant-value))
          (else (error "no operation by that name" op)))))


(define (multiplier m1-wire m2-wire p)
  
  
  (connect-to-wire )
  
  
  )





;
(define y (make-wire '()))
(define x (make-wire 5))
(define c (make-wire))
(define a (make-wire 2))
(define b (make-wire '()))



  
                  
                  
                  