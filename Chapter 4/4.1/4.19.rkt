;Exercise 4.19.  Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the desired result of evaluating the expression

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;Ben asserts that the result should be obtained using the sequential rule for define: b is defined to be 11, then a is defined to be 5, so the result is 16. Alyssa objects that mutual recursion requires the simultaneous scope rule for internal procedure definitions, and that it is unreasonable to treat procedure names differently from other names. Thus, she argues for the mechanism implemented in exercise 4.16. This would lead to a being unassigned at the time that the value for b is to be computed. Hence, in Alyssa's view the procedure should produce an error. Eva has a third opinion. She says that if the definitions of a and b are truly meant to be simultaneous, then the value 5 for a should be used in evaluating b. Hence, in Eva's view a should be 5, b should be 15, and the result should be 20. Which (if any) of these viewpoints do you support? Can you devise a way to implement internal definitions so that they behave as Eva prefers?26 

; sequential rules
(let ((a 1))            ; a = 1
  (define (f x)         ; x = 10
    (define b (+ a x))  ; b = 11
    (define a 5)        ; a = 5
    (+ a b))            ; result = 16
  (f 10))               

; simultaneous scope rules
(let ((a 1))
  (define (f x)
    (define b (+ a x))   ; b can't refer to a because they defined at the same time. 
    (define a 5)
    (+ a b))
  (f 10))
; turns into....
(let ((a 1))
  (define (f x)
    (let ((b '*unassigned*)
          (a '*unassigned*))
      (set! b (+ a x))
      (set! a 5)
      (+ a b)))
  (f 10))

; truly simultaneous
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))


; I like the idea of simultaneous scope rules best. With sequential rules, the order in which things are
; defined changes what the result is. 

; but what's a way to make the truly simultaneous way work? Or can we prove it's impossible with an example?

(define a (* 2 b))
(define b (- a 1))

; this is looking like 3.3.5 Propagation of Constraints. It's like a feedback system, where initial conditions
; may make the system converge or diverge. Let's say we start with b = 0;

b = 0
a = 0
b = -1
a = -2
b = -3
a = -6
b = -7
; okay this is divergent, what about this
b = 4
a = 8
b = 7
a = 14
b = 13
; also divergent, but what about this!
b = 1
a = 2
b = 1
; convergent.


