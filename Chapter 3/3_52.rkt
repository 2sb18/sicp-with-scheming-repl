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
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else
          (display "enumerate ") (display start) (newline)
          (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))


; this takes in a stream and outputs a stream
(define (stream-filter predicate stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((predicate (stream-car stream))
         (cons-stream (stream-car stream)
               (stream-filter predicate (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

;Exercise 3.52.  Consider the sequence of expressions

(define sum 0)
; sum = 0
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; stream-enumerate-interval = '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
                                                                 ; seq = '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
; seq = (stream-map accum (cons 1 (delay (stream-enumerate-interval 2 20))))
;     = (cons (accum 1) (delay (stream-map accum (cons 2 (delay (stream-enumerate-interval 3 20)))
;     = (cons 1         (delay (stream-map accum (cons 2 (delay (stream-enumerate-interval 3 20)))
; sum = 1 
(define y (stream-filter even? seq)) ; y = '(6 10 28 36 66 78)
; y = (cons 6 (stream-filter even? (stream-map accum (stream-enumerate 4 20))))
; y = (cons 6 (stream-filter even? (stream-map accum (cons 4 (delay (stream-enumerate 5 20))))))
; y = (cons 6 (delay (stream-filter even? (cons 10 (delay (stream-map accum (cons 5 (delay (stream-enumerate 6 20)))))))))
; sum = 10
; actually, running the code up to this point reveals that sum = 6. (accum 4) hasn't been done yet
; why not???
; Answer: a (delay blah blah) around a bunch of stuff means that none of that stuff will be executed.
; I thought that with something like (delay (stream-filter even? blah blah)) the stream-filter would
; be executed, but it isn't. So the above is incorrect. It should be:
; y = (stream-filter even? (stream-map accum (stream-enumerate-interval 1 20)))
; y = (stream-filter even? (stream-map accum (cons 1 (delay (stream-enumerate-interval 2 20)))))
; (accum 1)
; y = (stream-filter even? (cons 1 (delay (stream-map accum (stream-enumerate-interval 2 20)))))
; y = (stream-filter even? (stream-map accum (stream-enumerate-interval 2 20)))))
; y = (stream-filter even? (stream-map accum (cons 2 (delay (stream-enumerate-interval 3 20)))))
; y = (stream-filter even? (cons 3 (delay (stream-map accum (stream-enumerate-interval 3 20)))))
; y = (stream-filter even? (stream-map accum (stream-enumerate-interval 3 20)))
; ...
; y = (cons 6 (delay (stream-filter even? (stream-map accum (stream-enumerate-interval 4 20)))))
; 

; 

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq)) ; filter is "divisible by 5?"
; seq = '(1 3 6 10 15 21 28 
; z = '(10 15 ...)
; first thing we get to is 10 at this point sum = 10.
; RIGHT!
(stream-ref y 7)
; at this point sum = 136
; CORRECT!
(display-stream z)
; at this point sum = 210
; CORRECT!

;What is the value of sum after each of the above expressions is evaluated? What is the printed response to evaluating the stream-ref and display-stream expressions? Would these responses differ if we had implemented (delay <exp>) simply as (lambda () <exp>) without using the optimization provided by memo-proc ? Explain. 

; no, 






; extra thinking
(define mult_calls 0)
(define sqre_calls 0)

(define (mult x)
  (display "mult ") (display x) (newline)
  (* 2 x))
(define (sqre x)
  (display "sqre ") (display x) (newline)
  (* x x))

(define seqy (stream-map sqre (stream-map mult (stream-enumerate-interval 1 5))))
; enumerate-interval '(1  2  3  4   5)
; mult               '(2  4  6  8  10)
; sqre               '(4 16 36 64 100)
; compiler says after this we only have 1 mult call and 1 sqre call
; how does this work?
;
; enumerate happens to produce 1
; seqy = (stream-map sqre (stream-map mult (cons 1 (delay (stream-enumerate-interval 2 5)))))
; (mult 1 happens) 
; seqy = (stream-map sqre (cons 2 (delay (stream-map mult (cons 2 (stream-enumerate-interval 3 5)))))) 

