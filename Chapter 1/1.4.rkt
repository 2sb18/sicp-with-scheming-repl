; if b is greater than 0, we're going to use the + operator
; if b is less than 0, we're going to use the - operator
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; in C
; 
; int a_plus_abs_b ( int a, int b ) {
;   if ( b > 0 ) {
;     return a + b;
;   } else {
;     return a - b;
;   }
; }

