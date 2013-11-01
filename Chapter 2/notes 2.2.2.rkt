#lang planet neil/sicp

(define (pl n)
  (define (loop n)
    (display (car n))
    (cond ((null? (cdr n)) 0)
          (else (display " ") (loop (cdr n)))))
  (display "(")
  (loop n)
  (display ")"))

; procedure is a function which takes in an element of the list and produces an
; output. items is a list
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) 
            (map proc (cdr items)))))


(define (for-each proc items)
  (if (null? items)
      nil
      ((lambda ()
        (proc (car items))
        (for-each proc (cdr items))))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
