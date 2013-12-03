; Exercise 4.15.  Given a one-argument procedure p and an object a, p is said to ``halt'' on a if evaluating the expression (p a) returns a value (as opposed to terminating with an error message or running forever). Show that it is impossible to write a procedure halts? that correctly determines whether p halts on a for any procedure p and object a. Use the following reasoning: If you had such a procedure halts?, you could implement the following program: 

; the run-forever program just calls itself over and over again
(define (run-forever) (run-forever))

; 'try' takes a procedure p (which takes one argument) and sends it
; to halts? as the procedure and argument 
; halts? returns 1 if the procedure and object halts, and 0 if it doesn't
; try will halt if (p p) wouldn't halt. try will run-forever if (p p) halts.
(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted))

; what happens when (try try) is executed?

; (try try) will halt if (try try) wouldn't halt. (try try) will run-forever if
; (try try) halts.

; so would (halts? try try) return 1 or zero?

; halts? is a program, just like any other program, if it exists, you can use it
; in another program.
