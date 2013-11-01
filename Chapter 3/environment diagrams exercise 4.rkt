;(define (kons a b)
;  (lambda (m)
;    (if (eq? m ’kar) a b) ))

(define kons
  (lambda (a b)
    (lambda (m)
      (if (eq? m 'kar)
          a
          b))))

(define p (kons (kons 1 2) 3))

(p 0)  ; should return 3

(p 'kar) ; returning P2

((p 'kar) 0) ; returns 2

((p 'kar) 'kar)  ; returns 1

;- got to run (define p (kons (kons 1 2) 3))
; - first we got to run the procedural call (kons (kons 1 2) 3)
;   - got to run procedural call (kons 1 2)
;     - create frame E1, pointing to global.
;     - a and b are 1 and 2
;     - run the code, which is a lambda, which creates a procedure.
;       - this procedure points to E1.
;       - this procedure, called P2, is returned by the procedure call
;   - now we call the outside kons
;    - create a frame, which points to global.
;    - a points to P2, b is 3
;    - from E2, run the kons code.
;      - it's a lambda, so we create new procedure P3, which points to E2
;- now we bind p to P3


    