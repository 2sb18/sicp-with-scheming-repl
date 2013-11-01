(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; can this function be used concurrently by multiple people with multiple accounts?

; I think Ben is right on this one, this function works fine. The difference between
; this and the exchange function is that the exchange function had to calculate the
; difference, and then use that difference to do the withdraw and deposit. Because
; the amounts in the accounts could change after the difference was calculated, it
; could be out of date.

; But with this function, there's no way for any information to be out of date, other
; processes don't affect the amount that has to be transferred. This function is
; assuming though that it is possible to have negative amounts in accounts.
