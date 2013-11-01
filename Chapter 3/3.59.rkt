

#lang planet neil/sicp


(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (car-stream s))
           (stream-for-each proc (cdr-stream s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-ref s n)
  (if (= n 0)
    (car-stream s)
    (stream-ref (cdr-stream s) (- n 1))))

(define (car-stream s)
  (car s))

(define (cdr-stream s)
;  (display "cdr-stream happening\n")
  (force (cdr s)))

(define (stream-enumerate-interval start end)
  (cond ((> start end)
         the-empty-stream)
        (else
          (cons-stream start (stream-enumerate-interval (+ start 1)
                                                            end)))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
    the-empty-stream
    (begin (display "mapping\n")
           (cons-stream (apply proc (map car-stream streams))
                        (apply stream-map (cons proc (map cdr-stream streams)))))))


; this takes in a stream and outputs a stream
(define (stream-filter predicate stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((predicate (car-stream stream))
         (cons-stream (car-stream stream)
               (stream-filter predicate (cdr-stream stream))))
        (else (stream-filter predicate (cdr-stream stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (display-stream-partial stream start end)
  (display (stream-ref stream start))
  (newline)
  (if (not (eq? start end))
    (display-stream-partial stream (+ 1 start) end)))

; input starts with the constant
; output starts with the first non-constant (which is the constant
; that goes along with x)
(define (integrate-series power-stream)
  (define (iter stream index)
   (if (stream-null? stream)
    'done
    (cons-stream (/ (car-stream stream) (+ 1 index))
                 (iter (cdr-stream stream) (+ 1 index)))))
  (iter power-stream 0))

(define (add-one-to-stream stream)
  (cons-stream (+ 1 (car-stream stream))
               (add-one-to-stream (cdr-stream stream))))

(define series (cons-stream 0 (add-one-to-stream series)))

; in the following we don't need external procedures
(define series2 (cons-stream 0
                             ((lambda (stream)
                                (define (add-one stream)
                                  (cons-stream (+ 1 (car-stream stream))
                                               (add-one (cdr-stream stream))))
                                (add-one stream)) series2)))

(define ones (cons-stream 1 ones))
(define integrated (integrate-series ones))
;(display-stream-partial integrated 0 10)

(define exp-series (cons-stream 1 (integrate-series exp-series)))

;(display-stream-partial exp-series 0 10)

(define (negate-stream stream)
  (cons-stream (* -1 (car-stream stream))
               (negate-stream (cdr-stream stream))))

(define cosine-series
  (cons-stream 1 (negate-stream (integrate-series (cons-stream 0 (integrate-series cosine-series))))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(display-stream-partial cosine-series 0 10)
(newline)
(display-stream-partial sine-series 0 10)



