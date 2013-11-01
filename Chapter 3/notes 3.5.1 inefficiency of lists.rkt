(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; we want to compute the sum of all teh prime numbers in an interval.

; here's the standard iterative style:
; this goes through each number, one by one, looks at whether it's prime, and if it
; is, it adds it to accum
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))



; here's a style using filter and accumulate, list manipulation.
; this creates a list from a to b. Then it takes that list and filters
; out the numbers that aren't prime, creating a new list. Then it goes through
; that list and adds together all the elements.
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

; iterative style
; pro: more computationally efficient
; con: harder to read

; list manipulation style
; pro: easier to read and reason about
; con: inefficient.

; streams
; pros: looks like list manipulation, but computated
;       incrementally

; computing the second prime in the interval from 10,000 to 1,000,000
; 
(car (cdr (filter prime? (enumerate-interval 10000 1000000))))
