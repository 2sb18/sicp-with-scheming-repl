; queue operations
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue)
  (cons nil nil))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "front-queue called on an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((item-cons (cons item nil)))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue item-cons)
               (set-rear-ptr! queue item-cons)
               queue)
        (begin (set-cdr! (rear-ptr queue) item-cons)
               (set-rear-ptr! queue item-cons)
               queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "delete-queue! called on an empty queue" queue)
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))

(define (print-queue queue)
  (front-ptr queue))

(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(delete-queue! q1)

; this last procedure call will return a queue that has a value of nil
; for the front pointer, but still has the rear pointer pointing to 'b.
; since the front pointer is nill, the queue is empty.






   
