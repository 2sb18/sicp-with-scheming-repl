#lang planet neil/sicp 

(#%require "streams.rkt")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (display "improving\n")
  (average guess (/ x guess)))

(define (cons-stream a b)
  (cons a (lambda () b)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(stream-ref (sqrt-stream 2) 5)

; what happens when we call (sqrt-stream 2) with this?
; (stream-ref (sqrt-stream 2) 4)
; the (sqrt-stream 2) is executed first
; the guesses variable code is run.
; so (cons-stream 1.0 (stream-map (blah blah))) is run, resulting in
; guesses being binded to (1.0 (delay (stream-map (blah blah))))
; a reference to the guesses variable is returned by the (sqrt-stream 2) procedure
; so now we have (stream-ref g1 4) where guesses = g1 = (cons 1.0 (delay (blah blah)))
; this calls (stream-ref (stream-cdr g1) 3)
; so g2 = (stream-cdr g1) results in (stream-map (lambda (guess) (sqrt-improve guess x) guesses))
; which results in g2 = (1.5 (delay (stream-map (lambda (guess) (sqrt-improve guess x) (stream-cdr guesses))))) 

; here's where the efficiency comes in. see at the end, the (stream-cdr guesses)? Instead of having to
; do that calculation again, the memo-proc has already saved the answer for us!!!!

; so to calculate g3 (third element in the stream), with memo-proc, we have to calcuate:
; g1, g2, g3

; without memo-proc, we have to calcuate:
; g1
; calculate g2, which requires g1
; calculate g3, which requires g2, which requires g1.
; 6 calculations all together.
; if memo-proc isn't available, having the local variable doesn't make a difference.

;Exercise 3.63.  Louis Reasoner asks why the sqrt-stream procedure was not written in the following more straightforward way, without the local variable guesses:

(define (sqrt-stream2 x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream2 x))))

(stream-ref (sqrt-stream2 2) 5)

; what would happen here?
; call (stream-ref (sqrt-stream 2) 4)
; (sqrt-stream 2) returns g1 = (cons 1.0 (delay (stream-map proc (sqrt-stream 2))))
; so now we have (stream-ref g1 4)
; this turns into (stream-ref (stream-cdr g1) 3)
; (stream-cdr g1) turns into (stream-map proc (sqrt-stream 2))
; which results in (sqrt-stream 2) being called, which returns (cons 1.0 (delay (stream-map proc (sqrt-stream 2))))
; this has to be calculated, which is what we're calculating! definitely inefficient. 
; so we have to calculate g1 again, so we have
; g2 = (cons 1.0 (delay (stream-map proc g1)))
; (stream-ref g2 3) turns into (stream-ref (stream-cdr g2) 2)
; (stream-cdr g2) tu
;   

; so to calculate g4, you'd have to calculate
; g1 first
; then g2, which means calculating g1 first
; then g3, which means g2, then g1
; then g4, which means g3, g2, and g1

;Alyssa P. Hacker replies that this version of the procedure is considerably less efficient because it performs redundant computation. Explain Alyssa's answer. Would the two versions still differ in efficiency if our implementation of delay used only (lambda () <exp>) without using the optimization provided by memo-proc (section 3.5.1)? 
