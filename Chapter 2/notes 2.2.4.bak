#lang planet neil/sicp

; print list, can be nested
; makes all the spacing correct!
(define (pl n)
  (define (in-loop n space?)
    (cond ((null? n) 0)
          ((not (pair? n)) (if space? (display " ")) (display n))
          ((list? (car n)) (out-loop (car n) #t) (in-loop (cdr n) #t))
          (else (if space? (display " ")) (display (car n)) (in-loop (cdr n) #t)))) 
  (define (out-loop n space?)
    (cond ((list? n) (if space? (display " ")) (display "(") (in-loop n #f) (display ")"))
          (else (in-loop n #f))))
  (out-loop n #f) (display "\n"))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; putting an element from the seq through proc has to
; result in a list.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define empty-board '())

(define (get-position col positions)
  (cond ((not (pair? positions)) #f)
        ((= col 1) (car positions))
        (else (get-position (- col 1) (cdr positions)))))
  

(define (safe? k positions)
  (let ((k-position (get-position k positions)))
    (define (check-from-column col)
      (let ((column-position (get-position col positions)))
        (cond ((= col k) #t)
              ((or (= column-position k-position)
                   (= column-position (+ k-position (- k col)))
                   (= column-position (- k-position (- k col))))
               #f)
              (else (check-from-column (+ col 1))))))
    (check-from-column 1)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)  ; place the queen in the k column, returns a list of lists
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; in queens, the procedue queen-cols calls itself once, so (queen-cols 8) calls
; (queen-cols 7) calls (queen-cols 6) down to (queen-cols 0)
; so lets pretend queen-cols taks time t, the whole function takes 8t.

; in queens-crap, the procedue queen-cols calls itself 8 times. (queens-cols 8) makes
; 8 separate calls to (queen-cols 7), which makes 8 separate calls to (queens-cols 6),
; and so forth. almost 512 calls altogether. 64 times longer! 64T

(define (queens-crap board-size)
  (define (queen-cols k)  ; place the queen in the k column, returns a list of lists
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(runtime)
(pl (queens 8))     ; took 198000
(runtime)
(pl (queens-crap 8)) ; took 64880000,  330 times longer
(runtime)











      












       