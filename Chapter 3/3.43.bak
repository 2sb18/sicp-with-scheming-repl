(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

; serialized-exchange is supposed to not operate concurrently with any operations
; that happen with account1 and account2

; (serializer2 exchange) makes sure that the exchange procedure doesn't happen
; concurrently with other serializer2 operations. Same goes for serializer1.
; Now we run the exchange procedure, which won't run concurrently with the other
; account1 or account2 operations.