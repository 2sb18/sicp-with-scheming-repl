#lang racket

; thread all the procs
; wait until all the threads are complete
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

(thread (lambda ()
          
          (for ([i 10])
            (sleep 2)
            (printf "thread 1\n"))))


(thread (lambda ()
          (for ([i 20])
            (sleep 1)
            (printf "thread 2\n"))))

(define y [* 2 x])