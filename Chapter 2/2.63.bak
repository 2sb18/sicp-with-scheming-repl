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

; this is O(n)
; set is ordered from smallest to biggest
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((> x (car set)) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set-old x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; this is O(n)
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))


; set must be ordered
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; this is O(n) for two sets of size n
; union contains all the elements in either set

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((equal? x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 (else (cons x2 (union-set set1 (cdr set2)))))))))

(pl (union-set '(1 3 6 7 8 9 11) '(2 3 5 7 8 10 14)))











      












       