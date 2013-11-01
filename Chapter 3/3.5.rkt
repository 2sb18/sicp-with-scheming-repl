(#%require (only racket/base random))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (random 1000000) (random 1000000)) 1))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (- x2 x1) (- y2 y1) 
     (monte-carlo trials 
                  (lambda ()
                    (let ((x (random-in-range x1 x2))
                          (y (random-in-range y1 y2)))
                      (P x y))))))

(define (square x) (* x x))

(define (estimate-pi-integral trials)
  (/ (estimate-integral (lambda (x y)
                       (<= (+ (square x) (square y)) (square 1000000)))
                     -1000000
                     1000000
                     -1000000
                     1000000
                     trials)
     (square 1000000.0)))

(estimate-pi-integral 1000000)
                    

  
