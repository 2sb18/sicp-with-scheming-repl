#lang planet neil/sicp

; implementation layer

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; user layer

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
        
(define (div-interval x y)
  (if (and (> 0 (lower-bound y)) (< 0 (upper-bound y)))
      (error "divisor spans 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (width-interval x)
  (/
   (- (upper-bound x) (lower-bound x))
   2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-interval (* (- 100 percent) center 0.01)
                 (* (+ 100 percent) center 0.01)))
(define (percent x)
  (* 100 (/ (width-interval x) (center x))))

(define x (make-center-percent 1000 5))
(define y (make-center-percent 500 1))

(div-interval x x)
(div-interval x y)

; dividing x by x should lead to 1 with no error interval, because we're using
; the same resistor for both numerator and denominator. but our div-interval
; is treating the numerator and denominator as different resistors.