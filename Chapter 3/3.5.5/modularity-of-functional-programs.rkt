
#lang planet neil/sicp 

(#%require "../streams.rkt")

(define random-init 17)

; from K&R
; int rand() {  
;   random_seed = random_seed * 1103515245 +12345;   
;   return (unsigned int)(random_seed / 65536) % 32768; 
; }
(define (rand-update x)
  (let ((y (+ 12345 (* x 1103515245 ))))
    (modulo (floor (/ y 54536)) 32768)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

; s is a stream of numbers
; f is a function that takes two arguments
; this function takes two numbers from the stream, and
; applies them to f.
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

; if the pair of numbers have no common denominators, cesaro
; is one, if they do, 0 
; so this is a stream of 1s and 0s, testing random numbers to
; see if there are relatively prime.
(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

; experiment-stream is either 1 for pass or 0 for fail
; returns a stream of the fraction of successes
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              ; this gives a stream of increasing accuracy of
              ; whether two random numbers are relatively prime.
              (monte-carlo cesaro-stream 0 0)))

(stream-ref pi 10000)
