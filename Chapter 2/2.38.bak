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

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))


(define (transpose mat)
  (accumulate-n cons '() mat))

(pl (transpose (list (list 1 2 3) (list 4 5 6))))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) 
           (map (lambda (n-column)
                  (dot-product m-row n-column)) cols)) m)))

; this should be ( (19 22) (43 50))
(pl (matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))


          









       