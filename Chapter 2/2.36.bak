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

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate 
   (lambda (x y) (+ y x)) 
   0 
   (map (lambda (x)
          (if (pair? x)
              (count-leaves x)
              1)) t)))

(define (count-leaves-old x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves-old (car x))
                 (count-leaves-old (cdr x))))))

(count-leaves-old (list 1 2 (list 3 4 (list 5)) 4))

(count-leaves (list 1 2 (list 3 4 (list 5)) 4))



          









       