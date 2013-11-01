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

; deriv system with message passing

; kinda like it. implementation is totally hidden. Don't have to rely on
; the get/put table

(define (deriv exp var)
  (exp 'deriv var))

(define (show exp)
  (exp 'show))

(define (make-number num)
  (define (dispatch op . args)
    (cond ((not (number? num)) (error "num isn't a number -- MAKE-NUMBER" num))
          ((eq? op 'deriv) (make-number 0))
          ((eq? op 'show) num)))
  dispatch)

(define (make-symbol sym)
  (define (dispatch op . args)
    (cond ((not (symbol? sym)) (error "sym isn't a symbol -- MAKE-SYMBOL" sym))
          ((eq? op 'deriv) (if (eq? sym (car args)) (make-number 1) (make-number 0)))
          ((eq? op 'show) sym)))
  dispatch)

(define (make-sum exp1 exp2)
  (define (dispatch op . args)
    (cond 
      ((eq? op 'deriv) (make-sum (deriv exp1 (car args)) (deriv exp2 (car args))))
      ((eq? op 'show) (list '+ (show exp1) (show exp2)))))
  (cond ((eq? 0 (show exp1)) exp2)
        ((eq? 0 (show exp2)) exp1)
        ((eq? (show exp1) (show exp2)) (make-product (make-number 2) exp))
        (else dispatch)))
  
(define (make-product exp1 exp2)
  (define (dispatch op . args)
    (cond ((eq? op 'deriv) (make-sum (make-product exp1 (deriv exp2 (car args)))
                                     (make-product exp2 (deriv exp1 (car args)))))
          ((eq? op 'show) (list '* (show exp1) (show exp2)))))
  (cond ((or (eq? 0 (show exp1)) (eq? 0 (show exp2))) (make-number 0))
        ((eq? 1 (show exp1)) exp2)
        ((eq? 1 (show exp2)) exp1)
        (else dispatch)))


(deriv (make-symbol 'x) 'x)
(pl (show (make-sum (make-number 3) (make-number 4))))

(pl (show (make-sum (make-product (make-number 3) (make-symbol 'x))
                    (make-symbol 'y))))

(pl (show (deriv (make-sum (make-symbol 'x) (make-number 3)) 'x)))

(pl (show (make-product (make-number 0) (make-symbol 'x))))
(pl (show (make-product (make-number 1) (make-symbol 'x))))


