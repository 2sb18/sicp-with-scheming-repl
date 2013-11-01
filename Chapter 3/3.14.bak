(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

; now the last pair of z points to it's first pair, so calling
; (last-pair z) leads to an infinite loop

(last-pair z)