
;- g is (lambda (x) (* x 3))
;- returned is (lambda (y) ((lambda (x) (* x 3)) (- y 6)))
;
;
;
;- evaluate 

(foo 5) ; will return ((lambda (x) (* x 3)) (- 5 6)) = -3


(define foo
  ((lambda (g)
     (lambda (y) (g (- y 6))))
   (lambda (x) (* x 3))))

- the 2nd argument of the define is evaluated. It's a procedural call, but first
  the arguments must be evaluated
  - first argument is (lambda (g) (lambda (y) (g (- y 6))))
    - P1 bubble-pair is created
  - second argument is (lambda (x) (* x 3))
    - P2 bubble-pair is created
- now that the arguments of the procedural call have been evaluated, we can 
  evaulate the procedural call
  - E1 is created, which points to global frame.
  - in E1, P1 is executed.
  - in E1, g points to P2
  - a lambda is evaluated, so a new bubble-pair is created, P3
    - we're in E1 right now, so P3 points to E1
- foo is bound to P3

- (foo 5) is called, creating E2, which points to E1
  - y is set to 5 in E2.
  - P3 code is evaluated in E2.
    - (g -1) is evaluated, new frame E3 is created which points to global
      and x is bound to -1 in E3.
      - P2 is evaluated in E3, which results in -3.
