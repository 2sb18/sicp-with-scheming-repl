(define (A x y)
  (cond ((= y 0) 0)                   ; if y is 0, return 0
        ((= x 0) (* 2 y))             ; if x is 0, return 2 times y
        ((= y 1) 2)                   ; if y is 1, return 2
        (else (A (- x 1)              ; else call A again reducing x by 1
                 (A x (- y 1))))))    ; and turning y into (A x (y-1))

(define (f n) (A 0 n))      ; f(n) = 2*n 
(define (g n) (A 1 n))      ; g(n) = 2^n
(define (h n) (A 2 n))      ; h(n) = [h(n-1)]^2 = 2^(2^(2^(n-2)))
                            ;                   = 8^(n-2)
; h(0) = 0
; h(1) = 2      2^1   = 2^(2^0) =
; h(2) = 4      2^2   = 2^(2^1) = 2^(2^(2^0))
; h(3) = 16     2^4   = 2^(2^2) = 2^(2^(2^1))
; h(4) = 65536  2^16  = 2^(2^4) = 2^(2^(2^2))
; h(n) = PRODUCT from 0 to n 
; does 2^16 = 16^2? yes
; does 2^4 = 4^2? yes
; does 2^3 = 3^2? no

(define (k n) (* 5 n n))