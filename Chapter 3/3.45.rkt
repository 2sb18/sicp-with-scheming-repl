(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
 ((account 'deposit) amount))

; so now we got the following functions in the serializer
; - withdraw
; - deposit
; - exchange

; so we call serialized-exchange.
; first it says to the serializer, "don't let anyone use you till I'm done, bitch!"
; then it get's the balances. easy.
; then it calculates the difference. good
; then it attempts a serialized withdraw, not going to happen, cause it has locked
;     up the serializer! We're stuck!


