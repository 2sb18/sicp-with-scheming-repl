(define count
  ((lambda (result)
     (lambda ()
       (set! result (+ result 1))
       result))
   0))

(count)

What the fuck is count? When you evaluate the 2nd argument of count, it returns an
anonymous function (lambda () (set! result (+ result 1)) result). So when you call count,
this anonymous function is invoked.


- (define...), with a define, we evaluate the 2nd argument, then bind it to a name
  - evaluating the ((lambda (result) ...) 0), which is a procedure call. first we're
    going to have to create the procedure, then invoke it
    - create a pair-bubble for the procedure, call it P1. p: result, b: (lambda () (set! ...) result)
    - procedure points to global frame
    - invoking procedure. creating new frame E1, points to global environment, in
      frame making result: 0. 
      - executing procedure P1 from E1. We come upon a lambda. This creates a procedure
        - evaluating procedure (lambda () (set! result (+ result 1)) result)
          - no params, b: (set! result (+ result 1))
          - points to E1?

- (count)


