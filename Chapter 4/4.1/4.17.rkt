;Exercise 4.17.  Draw diagrams of the environment in effect when evaluating the expression <e3> in the procedure in the text, comparing how this will be structured when definitions are interpreted sequentially with how it will be structured if definitions are scanned out as described. Why is there an extra frame in the transformed program? Explain why this difference in environment structure can never make a difference in the behavior of a correct program. Design a way to make the interpreter implement the ``simultaneous'' scope rule for internal definitions without constructing the extra frame. 


; without scan-out-defines
; (define meow (lambda (x) (define u 3) (define v 4) (* x u v)))
; (meow 7)


;                                  +---------------------------------+
;                                  |                                 |
; the-global-environment +-------->|                                 |
;                                  |                                 |
;                                  |                                 |
;                                  |     meow+--+                    |
;                                  |            |                    |
;                                  +------------|-----+--------------+
;                                               |     ^            ^
;                                               |     |            |    (meow 7)
;                                               v     |         +--+----------+
;                                        procedure+---+         |             |
;                                        +                      | x: 7        |
;                                        |                      | u: 3        |
;                                        |                      | v: 4        |
;                                        v                      |             |
;                                      p:x                      |             |
;                                      c: (define u 3)          |             |
;                                         (define v 4)          +-------------+
;                                         (* x u v)                   returns 84


; with scan-out-defines
;                          +----------+the-global-environment
; +------------------------v---+
; |                            |
; |                            <------------------+
; |                            |                  |                        (created by let)
; |  meow                      |                  |                +------------------+
; |   +                        |         +--------+-+              | u: '*unassigned* |
; +---|----^-------------------+         |  x:7     <--------------+    then 3        |
;     |    |                             |          |              | v: '*unassigned* |
;     |    |                             +-------^--+              |    then 4        |
;     |    |                                     |                 |                  |
;     v    +                                     |                 +------------------+
;   procedure                                    +                           returns 84
;    +                                     procedure
;    |                                      +    (created by let)
;    v                                      |
;   p:x                                     v
;   c: (let ((u '*unassigned*)             p: u, v
;            (v '*unassigned*))            c: (set! u 3)
;         (set! u 3)                          (set! v 4)
;         (set! v 4)                          (* x u v)
;         (* x u v))

; there's an extra frame in the transformed program because the let is a lambda application in disguise. applications create frames

; this transformation can't make a difference in the operation of a correct program because it is completely separate from the rest of the environment

; to design a transformation that doesn't create an extra frame, could we just have the transformer rearrange the body so that all the defines are at the top? yes, that it was other internet solutions suggest


