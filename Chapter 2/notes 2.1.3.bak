#lang planet neil/sicp

; implementation layer
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

; user layer

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; rectangle implementation 1
#|
(define (make-rectangle center-point orientation-angle length width)
  (cons center-point (cons orientation-angle (cons length width))))
(define (length rectangle)
  (car (cdr (cdr rectangle))))
(define (width rectangle)
  (cdr (cdr (cdr rectangle))))
|#

; rectangle implementation 2: shitty three point implementation

(define (distance point-one point-two)
  (sqrt (+ (expt (- (x-point point-one) (x-point point-two)) 2) (expt (- (y-point point-one) (y-point point-two)) 2))))

(define (make-rectangle first-point second-point third-point)
  (cons first-point (cons second-point third-point)))
(define (length rectangle)
  (distance (car rectangle) (car (cdr rectangle))))
(define (width rectangle)
  (distance (car (cdr rectangle)) (cdr (cdr rectangle))))
  




(define (perimeter rectangle)
  (* 2 (+ (length rectangle) (width rectangle))))

(define (area rectangle)
  (* (length rectangle) (width rectangle)))


;(define rect (make-rectangle (make-point 3 4) 0.0 52.3 7.1))
(define rect (make-rectangle (make-point 0 0) (make-point 0 7.1) (make-point 52.3 7.1)))
(length rect)
(width rect)
(perimeter rect)
(area rect)






    

    
               







                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

