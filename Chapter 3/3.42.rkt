(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer))) ; protected is a serializer object
    ; now instead of creating a serialized procedure for every
    ; procedure call, we have one serialized withdraw and one serialized
    ; deposit that we always use.
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

; to withdraw
(define x (make-account 100))
((x 'withdraw) 10)   ; this calls the already created serialized withdraw procedure

; going to run into trouble when we have two people that want to withdraw at the same
; time. There's only one serialized withdraw procedure, what's going to happen?

; almost all websites that I checked say it's a safe change to make, but they don't
; address what happens when two deposits are made that the same time.

                       
                       
  



