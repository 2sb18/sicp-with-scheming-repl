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



; putting an element from the seq through proc has to
; result in a list.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

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

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-pairs-test n)
  (map (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(pl (unique-pairs-test 5))
(pl (unique-pairs 5))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (unique-triplets n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k)
                           (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
     
(define (unique-triplets-with-sum n s)
  (filter (lambda (x)
            (= s (+ (car x) (cadr x) (caddr x))))
          (unique-triplets n)))

(pl (unique-triplets-with-sum 100 10))






      












       