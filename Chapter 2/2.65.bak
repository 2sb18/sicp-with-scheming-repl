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

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (create-set list)
  (define (loop list result)
    (if (null? list)
        result
        (loop (cdr list) (adjoin-set (car list) result))))
  (loop list '()))

; I think this takes the same amount of time O(n)
; it's kinda half iterative
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; I believe list->tree is O(n)
; Bill the Lizard says I'm right!
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result)) ; get the left part of the tree
                (non-left-elts (cdr left-result)) ; this is the right, part, which is still a list
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts)) ; the middle part of the tree
                  (right-result (partial-tree (cdr non-left-elts) 
                                              right-size)))
              (let ((right-tree (car right-result)) ; right part of the tree
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) ; put together the tree
                      remaining-elts))))))))

(pl (list->tree '(1 3 5 7 9 11)))






      












       