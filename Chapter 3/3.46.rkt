(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; test-and-set! does the following:
; 1. accesses cell and gets the car of it
; 2. looks at that value, if it's true, return true
; if it's false 3. set car of cell to true
;               4. return false

; problem with this is that test-and-set! is not atomic.
; let's say two procedures are trying to acquire the mutex. they both call
; test-and-set! at the same time. they could both see that cell is false, then both
; set to true, and both procedures would run at the same time. No good!

