(define (cc amount)
  (define steps 0)
  (cc-recursive amount 5))

(define (cc-recursive amount kinds)
  (cond ((= amount 0) 1)
        ((= kinds 0) 0)
        ((> 0 amount) 0)
        (else (+ (cc-recursive amount (- kinds 1))
                 (cc-recursive (- amount (largest-denomination kinds)) kinds)))))

(define (largest-denomination kinds)
  (cond ((= kinds 0) 0)
        ((= kinds 1) 1)
        ((= kinds 2) 5)
        ((= kinds 3) 10)
        ((= kinds 4) 25)
        ((= kinds 5) 50)))

                              
                             
                              

      