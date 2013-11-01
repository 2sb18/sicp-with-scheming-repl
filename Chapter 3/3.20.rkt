; rewrote functions using lambdas
(define cons
  (lambda (x y)
    (define set-x! (lambda (v) (set! x v)))
    (define set-y! (lambda (v) (set! y v)))
    (define dispatch (lambda (m)
                       (cond ((eq? m 'car) x)
                             ((eq? m 'cdr) y)
                             ((eq? m 'set-car!) set-x!)
                             ((eq? m 'set-cdr!) set-y!)
                             (else (error "Undefined operation -- CONS" m)))))
    dispatch))

(define car (lambda (z) (z 'car)))
(define cdr (lambda (z) (z 'cdr)))
(define set-car! (lambda (z new-value)
                   ((z 'set-car!) new-value)
                   z))
(define set-cdr! (lambda (z new-value)
                   ((z 'set-cdr!) new-value)
                   z))

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)

#|

- first we come to the first lambda for the cons procedure
  - a bubble-pair is created P1
  - cons is created in the global frame and binded to P1
- same things happen for the procedures car, cdr, set-car!, and set-cdr!
- come to (define x (cons 1 2))
  - execute (cons 1 2)
    - create frame E1. cons points to Global so E1 does too
      - x:1 and y:2 in E1.
      - P1 code is executed from E1.
        - procedures P6, P7, and P8 are created and binded in E1 to set-x!, set-y!,
          dispatch.
        - dispatch is returned, so x in the global frame is binded to that dispatch
- come to (define z (cons x x))
  - execute (cons x x)
    - create frame E2, which points to global.
      - x points to P8, y points to P8.
      - P1 code is executed from E2.
        - P9, P10, and P11 are created and binded to names in E2.
        - dispatch is returned, so z in the global frame is binded to P11
- come to (set-car! (cdr z) 17)
  - first we gotta do the (cdr z)
    - frame E3 is created which points to the global environment, because cdr points
      to global environment.
      - z in E3 points to P11.
      - P3 code is executed in E3, the code being (z 'cdr)
        - a new environment is created, E4.
        - z in E3 points to P11, and P11 points to E2, so E4 points to E2.
        - m: 'cdr in E4
        - P11 code is executed from E4. returning y points to P8.
  - now we have (set-car! pointer-to-P8 17)
    - frame E5 is created for this procedural evaluation.
      - set-car! points to global, so E5 points to global
      - in E5, z points to P8, new value is 17
      - P4 code is executed in E5
        - the code is ((z 'set-car!) 17)
                      z
          - now we have to execute (z 'set-car!)
            - create frame E6.
            - z points to P8, which points to E1, so E6 points to E1.
            - m in E6 is 'set-car!
            - P8 code is executed in E6
              - returns a pointer to P6.
        - now we have (P6 17) z
          - new environment E7 is created that points to E1
          - v in E7 is 17
          - x in E1 is changed to 17
        - we're back to E5, where we return z, which is a pointer to P8
- last line is (car x)
  - created E8, z points to P8
  - P2 is executed from E8.
    - we get (z 'car), which is (P8 'car)
      - E9 is created, points to E1
      - m: 'car in E9
      - P8 is executed from E9
        - returns x, which is 17

|#
        
                      
        
      

