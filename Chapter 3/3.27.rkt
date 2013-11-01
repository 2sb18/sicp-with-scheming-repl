; returns the record of the corresponding key
; if nothing is found, returns false
(define (assoc key list-of-records)
  (cond ((null? list-of-records) #f)
        ((eq? key (caar list-of-records)) (car list-of-records))
        (else (assoc key (cdr list-of-records)))))
  

(define (lookup key table)
  (if (eq? (car table) '*table*)
      (let ((record (assoc key (cdr table))))
        (if record
            (car record)
            #f))
      (error "calling lookup procedure on a non-table" key table)))

; if record is found, replace value with new one
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


(define memoize
  (lambda (f)
    ((lambda (table)
       (lambda (x)
         ((lambda (previously-computed-result)
            (or previously-computed-result
                ((lambda (result)
                   (insert! x result table)
                   result) (f x)))) (lookup x table)))) (make-table))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 3)

- first there's a bunch of defines of lambda, even the (memoize f) is really
  returning a lambda, which leads to a bubble pair
- then we get to the (define memo-fib...)
  - we have to evaluate (memoize...)
    - this creates a frame E1, which points to global frame, cause the memoize
      procedure does.
      - now we have the (lambda (n) in the memoize). It's going to create a bubble
        pair P6 that points to E1. f in E1 will point to P6.
      - now memoize code is run from E1.
        -  memoize is a procedure that takes an argument f, in this case it's 
           a procedure from E1.
        - (make-table) is come apon.
          - This creates a new frame E2 which points to global cause make-table points
            to global.
          - make-table code is run from E2. Returns '(*table*)
        - we come across the (lambda (table)) code, so we have to create a bubble pair
        - now the (lambda (table) .... ) is run in the memoize procedure. This creates
          a bubble pair.
          - bubble pair P7 is created, points to E1, cause that's current frame.
        - P7 has to be evaluated with what we got returned from E2.
          - new frame E3 is created which points to E1, cause P7 points to E1.
            P7 code is run from E3.
            - P7 code is returning a lambda, so we gotta create a new bubble pair.
              - bubble pair P8 is created, pointing back to E3.
  - memo-fib points to P8
  
- (memo-fib 3) is evaluated
  - E4 is created. 
  - memo-fib points to P8, and P8 points to E3, so E4 points to E3.
  - in E4, x: 3
  - run P8 from E4.
    - first we have to evaluate (lookup x table)
      - E5 is created to do (lookup x table). x and table are filled in with 3 and
        the table in E3. E5 points to global, cause lookup does.
      - E5 returns #f
    - now we come upon the lambda C1.
      - new bubble pair P9 is created that points to E4.
      - we have to execute, so we create a new frame E5 which points to E4.
      - execute P9 from E6. in E6, previously-computed-result: #f
        - come upon ((lambda (result) (insert! x result table) result) (f x))
        - first we gotta execute (f x)
          - create new frame E7. f is found in E1, which points to P6, so E7 points
            to E1. In E7, n: 3, from E4.
          - P6 is executed from E7. which is (+ (memo-fib (- n 1)) (memo-fib (- n 2)))
          - (memo-fib 2) is evaluated
            - E8 is created, points to E3, x: 2. P8 is executed from E8.
              - E9 created for (lookup 2 D1). returns #f
            - we have the lambda C1. P10 is created, which points to C1 and E8.
            - have to execute P10, so create new frame E10 which points to E8.
              - in E10, previously-computed-result: #f from E9
              - execute P10 from E10
                - come upon ((lambda (result) (insert! x result table) result) (f x))
                - gotta execute (f x)
                  - create new frame E11. f is P6, which points to E1, so E11 points to E1.
                    n: 2 in E11
                  - P6 is executed from E11.  (+ (memo-fib (- n 1)) (memo-fib (- n 2)))
                  - (memo-fib 1) is evaluated
                    - E12 created, points to E3, x:1, P8 executed from E12.
                      - E13 created for (lookup 1 D1). returns #f
                      - lambda C1. P11 created, points to C1 and E12.
                      - create new frame E13, points to E12, pcr: #f. execute P11 from E13, 
                        - come upon ((lambda (result) (insert! x result table) result) (f x))
                          - gotta execute (f x)
                            - create E14. f is P6, E14 points to E1.
                            - P6 executed from E14. returns 1, current frame goes back to E13
                          - got (lambda (result) (insert! x result table) result)
                            - create P12, which points to E13.
                          - gotta execute P12. create frame E15, points to E13. result: 1 in E15
                            P12 is executed from E15
                            - (insert! x result table) has to be executed
                              - E16 points to global, has key: 1, value: 1, table: D1
                              - P3 is executed from E16.
                                - (1, 1) is added to D1
                            - result is returned from E15
                        - 1 is returned from E13
                    - 1 is returned by E12
                  - (memo-fib 0) is evaluated. A whole bunch of shit is created, in the end
                    0 -> 0 is added to the table.
                  - now we're back to P6 from E11. (+ 1 0). E11 returns 1.
                - back in E10, executing P10. The (f x) has resulted in 1.
                - got a (lambda (result) ...). bubble pair P13 points back to E10 and C2.
                - create E17. result: 1. Execute P13 from E17.
                  - got (insert! 2 1 D1). puts 2 -> 1 into table in E3. E10 returns 1.
            - back in E8 executing P8. E8 returns 1
          -  back in E7 executing P6. Now we have to get (memo-fib 1)
            - E18 created, pointing to E3, x: 1. executing P8 from E18.
              - have to run (lookup 1 D1)
                - creates some frames, end up returning 1.
              - P13 is created for lambda C1. points to E18
              - E19 is created to execute P13. pcr: 1. E19 returns 1.
              - E18 returns 1
            - E7 returns 3.
        - now we're back in E6 executing P9. The (f x) has resulted in 2.
        - some shit happens, 3 -> 2 gets put into D1.
  - 2 is returned for the (memo-fib 3)
  

        
                
                                     



