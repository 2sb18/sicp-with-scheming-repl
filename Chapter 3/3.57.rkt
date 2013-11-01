





#lang planet neil/sicp


(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
;  (display "stream-cdr happening\n")
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else
          (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
    the-empty-stream
    (begin (display "mapping\n")
           (cons-stream (apply proc (map stream-car streams))
                        (apply stream-map (cons proc (map stream-cdr streams)))))))


; this takes in a stream and outputs a stream
(define (stream-filter predicate stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((predicate (stream-car stream))
         (cons-stream (stream-car stream)
               (stream-filter predicate (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (display-stream-partial stream start end)
  (display (stream-ref stream start))
  (newline)
  (if (not (eq? start end))
    (display-stream-partial stream (+ 1 start) end)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(display-stream-partial fibs 0 20)
;Exercise 3.57.  How many additions are performed when we compute the nth Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number of additions would be exponentially greater if we had implemented (delay <exp>) simply as (lambda () <exp>), without using the optimization provided by the memo-proc procedure described in section 3.5.1.

; pretty sure cons-stream is a special form, so it happens right 
;fibs = (cons 0 (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;(define (stream-ref s n)
;  (if (= n 0)
;    (stream-car s)
;    (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc . streams)
;  (if (stream-null? (car streams))
;    the-empty-stream
;    (begin (display "mapping\n")
;           (cons-stream (apply proc (map stream-car streams))
;                        (apply stream-map (cons proc (map stream-cdr streams)))))) 


; what happens when we call (stream-ref fibs 3) without the memo-proc optimization
; without memo-proc optimization, fibs looks like this:
; fibs = (cons 0 (lambda () (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
; for the following, I'm going to be using c1 for the first cons in the fibs, c2 for the second, etc. 
; if we want to get the fourth fib number, ie (stream-ref fibs 3), this is what happens
; (stream-ref c1 3)
; (stream-ref (stream-cdr c1) 2)
; now the procedure at the cdr of c1 is executed, which is (cons-stream 1 (add-streams (stream-cdr c1) c1))
; this returns (cons 1 (lambda () (add-streams (stream-cdr c1) c1))) which we'll call c2.
; (stream-ref c2 2), which calls...
; (stream-ref (stream-cdr c2) 1)
; cdr of c2 is executed, which is (add-streams (stream-cdr c1) c1)
;   now we gotta do (stream-cdr c1), which we did before, but since there's no memoization, we gotta do it again.
;   leads to creating another cons cb2 which is a duplicate of c2, so we have (add-streams cb2 c1)
;   gotta execute (add-streams cb2 c1), which is really (stream-map + cb2 c1)
;     (cons-stream (+ (stream-car cb2) (stream-car c1)) (add-streams (stream-cdr cb2) (stream-cdr c1)))
;  c3 = (cons 1 (lambda () (add-streams (stream-cdr cb2) (stream-cdr c1))))
; (stream-ref c3 1)
; (stream-ref (stream-cdr c3) 0)

; I'm seeing a pattern here. If we uses memoization, it's O(n). There's only one addition for each new fib number

; If we don't have memoization, then it becomes a tree. How many total additions to get a certain number?
; no additions for first two numbers.
;for third number there's one addition.
;for fourth number, we have to get 2nd, where there's zero, and third, where's there's one. total = 1
;for fifth. fourth is 1 and third is 1. total = 2
;for sixth. fifth is 2 and fourth is 1. 3
; seventh. 5
; eighth. 8

; number of additions increases just like a fibonacci sequence, which is roughly exponential

; WHAT'S THE LESSON?
;
; if you don't use memo-proc optimization with a stream that references past values, then for every new
; value in the stream, it has to reference those past values, but since it didn't keep a record of them, 
; it has to reference values all the way back to the beginning of the stream!

