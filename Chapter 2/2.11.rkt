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

(define (neg? x)
  (< x 0))

(define (pos? x)
  (>= x 0))

(define (alt-mul-interval x y)
  (cond ((and (neg? (lower-bound x)) (neg? (upper-bound x)) (neg? (lower-bound y)) (neg? (upper-bound y))) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (neg? (lower-bound x)) (neg? (upper-bound x)) (neg? (lower-bound y)) (pos? (upper-bound y))) (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (neg? (lower-bound x)) (neg? (upper-bound x)) (pos? (lower-bound y)) (pos? (upper-bound y))) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
        ((and (neg? (lower-bound x)) (pos? (upper-bound x)) (neg? (lower-bound y)) (neg? (upper-bound y))) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (neg? (lower-bound x)) (pos? (upper-bound x)) (neg? (lower-bound y)) (pos? (upper-bound y))) (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                                                                                                                          (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))
        
        ((and (neg? (lower-bound x)) (pos? (upper-bound x)) (pos? (lower-bound y)) (pos? (upper-bound y))) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (pos? (lower-bound x)) (pos? (upper-bound x)) (neg? (lower-bound y)) (neg? (upper-bound y))) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (pos? (lower-bound x)) (pos? (upper-bound x)) (neg? (lower-bound y)) (pos? (upper-bound y))) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (pos? (lower-bound x)) (pos? (upper-bound x)) (pos? (lower-bound y)) (pos? (upper-bound y))) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        (else (error "alt-mult-interval error"))))
        
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


(define x (make-interval -3 -1))
(define y (make-interval 1 3))

(alt-mul-interval x y)

