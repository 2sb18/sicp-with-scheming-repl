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

(define (memq item seq)
  (cond ((null? seq) #f)
        ((eq? item (car seq)) seq)
        (else (memq item (cdr seq)))))

(pl (list 'a 'b 'c))                ; I'm guessing (a b c)

(pl (list (list 'george)) )         ; ((george))
(pl (cdr '((x1 x2) (y1 y2))))       ; ((y1 y2))

(pl (cadr '((x1 x2) (y1 y2))))      ; (y1 y2)
(pl (pair? (car '(a short list))))  ; #f
(pl (memq 'red '((red shoes) (blue socks)))) ; #f

(pl (memq 'red '(red shoes blue socks)))     ; (red shoes blue socks)

















      












       