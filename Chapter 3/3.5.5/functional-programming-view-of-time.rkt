
#lang planet neil/sicp 

(#%require "../streams.rkt")


(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

; balance is kept by the stream-withdraw, amount-stream
; is a stream with withdraw amounts.
(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))
