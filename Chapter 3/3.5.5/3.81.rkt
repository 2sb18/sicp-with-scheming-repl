
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

; ANSWER: we already have a complete stream answer!
; rand-update doesn't use set! to create the stream of
; random numbers
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))
