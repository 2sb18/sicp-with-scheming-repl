#lang racket


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; in normal order
= (gcd 206 40)
= (if (= 40 0)
      206
      (gcd 40 (remainder 206 40)))
= (gcd 40 (remainder 206 40))
= (if (= (remainder 206 40) 0)   ; remainders computed = 1
      40
      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
= (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
= (if (= (remainder 40 (remainder 206 40)) 0)    ; remainders computer = 3
      (remainder 206 40)
      (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
= (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
= (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)   ; remainders computed = 7
      (remainder 40 (remainder 206 40))
      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
= (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
= (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ; remainders computed = 7 + 7 = 14
      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; remainders computed = 14 + 4 = 18
      (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; 18 remainder functions computed all together

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; applicative-order evaluation?
= (gcd 206 40)
= (if (= 40 0)
      206
      (gcd 40 (remainder 206 40)))
= (gcd 40 (remainder 206 40))   ; remainders computed = 1
= (gcd 40 6)
= (if (= 6 0)
      40
      (gcd 6 (remainder 40 6)))
= (gcd 6 (remainder 40 6))   ; remainders computer = 2
= (gcd 6 4)
= (if (= 4 0)
      6
      (gcd 4 (remainder 6 4)))
= (gcd 4 (remainder 6 4))   ; remainders computed = 3
= (gcd 4 2)
= (if (= 2 0)
      4
      (gcd 2 (remainder 4 2)))
= (gcd 2 (remainder 4 2))     ; remainders computed = 4
= (gcd 2 0)
= (if (= 0 0)
      2
      (gcd 0 (remainder 2 0)))
= 2

; bill the lizard says this is correct ( the 18 times for normal-order, and 4 times for applicative-order