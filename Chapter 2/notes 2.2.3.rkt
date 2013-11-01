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

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (square x) (* x x))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(pl (even-fibs 10))
(sum-odd-squares (list (list 3 (list 1 4)) 2 5))

; return a list of integers from 1 to n
(define (integer-list n)
  (if (= 0 n)
      (list 0)
      (append (integer-list (- n 1)) (list n))))


; takes in a list, outputs a list with only the elements
; that pass the test.
(define (filter test n)
  (cond ((null? n) '())
        ((test (car n)) (cons (car n) (filter test (cdr n))))
        (else (filter test (cdr n)))))

; let's map first
; takes a nested or unnested list
(define (emfa input enumerator mappor filteror accumulator)
  (accumulator (filter filteror (map mappor (enumerator input)))))

; n is a list
(define (add-list n)
  (if (null? n) 0
      (+ (car n) (add-list (cdr n)))))

; n is a list
(define (flatten n)
  (cond ((null? n) '())
        ((not (pair? n)) (list n))
        (else (append (flatten (car n)) (flatten (cdr n))))))



(pl (emfa (list (list 3 (list 1 4)) 2 5) 
      flatten
      square
      odd?
      add-list))

(pl (emfa (integer-list 10)
      (lambda (x) x)
      fib
      even?
      (lambda (x) x)))

(pl (emfa (integer-list 10)
          (lambda (x) x)
          (lambda (x) (square (fib x)))
          (lambda (x) #t)
          (lambda (x) x)))

(define (product-of-squares-of-odd-elements sequence)
  (emfa sequence
        (lambda (x) x)
        square
        odd?
        (lambda (x)
          (define (mult x)
            (if (null? x)
                1
                (* (car x) (mult (cdr x)))))
          (mult x))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

          









       