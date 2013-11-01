;(define (make-counter)
;  (define result 0)
;  (lambda ()
;    (set! result (+ result 1))
;    result) )
;(define c1 (make-counter))
;(define c2 (make-counter))
;(c1)
;(c1)
;(c2)


(define make-counter
  (lambda ()
    (define result 0)
    (lambda ()
      (set ! result (+ result 1))
      result)))

(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c1)
(c2)

- come to the make-counter define. first we have to evaluate the 2nd argument, then
  bind to 'make-counter'
  - 2nd argument is that lambda on line 14. lambda creates procedure P1, binds it to
    'make-counter'
- come to (define c1 (make-counter)). First we have to evaluate 2nd argument, then
  bind it to c1.
  - from the global frame, we invoke (make-counter). create a frame E1. From E1 we
    execute the code.
    - The first line of code is (define result 0), so I'm guessing that means we add
      a value of 0 to E1 and bind it to result.
    - the second line of code is the lambda() thing, so we create a bubble-pair and
      call it P2
      - the procedure points to E1
      - the procedure is returned by the (make-counter)
  - the returned procedure is binded to c1
- same sort of thing happens with the (define c2 (make-counter)), creating P3 and E2
- (c1) is executed. That creates a new frame E3. c1 points to P2, which points to E1,
  so this new frame E3 points to E1
  - from E3, the body of P2 is executed. result in E1 is changed to 1
- (c1) is executed again, creating a new frame E4, which points to E1.
  - from E4, body of P2 is executed, result in E1 is changed to 2.
- (c2) is executed. E5 is created, which points to E2. result in E2 is changed to 1.