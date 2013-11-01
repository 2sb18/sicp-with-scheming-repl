; coins worth 1, 5, 10, 25, 50

(define (!= a b) (not (= a b)))

(define x 0)

(define (count-change amount)
  (define (cc amount denominations)
    (set! x (+ x 1))
    (cond ((= amount 0) 1)
          ((> 0 amount) 0)
          ((and (!= amount 0) (= denominations 0)) 0)
          (else (+ (cc (- amount (biggest-denomination denominations)) denominations)
                   (cc amount (- denominations 1))))))
  (define (biggest-denomination denominations)
    (cond ((= denominations 5) 50)
          ((= denominations 4) 25)
          ((= denominations 3) 10)
          ((= denominations 2) 5)
          ((= denominations 1) 1)
          (else 0)))
  (set! x 0)
  (cc amount 5))