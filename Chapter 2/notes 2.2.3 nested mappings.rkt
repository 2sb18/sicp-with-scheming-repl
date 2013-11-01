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

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))



(define (prime? n)
  (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; get all pairs of numbers where 1 <= j < i <= n
(define (all-pairs n)
  (accumulate append nil (map (lambda (i)
                                (map (lambda (j) (list i j))
                                     (enumerate-interval 1 (- i 1))))
                              (enumerate-interval 1 n))))

; putting an element from the seq through proc has to
; result in a list.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(pl (all-pairs 6))

(pl (flatmap (lambda (i)
               (map (lambda (j) (list i j))
                    (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 6)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (flatmap (lambda (i)
                                                   (map (lambda (j) (list i j))
                                                        (enumerate-interval 1 (- i 1))))
                                                 (enumerate-interval 1 n)))))

(pl (prime-sum-pairs 6))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))



(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x) s) s)))

(map (lambda (x) x) (list 1 2 3))

(pl (flatmap (lambda (x) (list x)) (list 1 2 3)))
      












       