; 1.12
; pascal's triangle

(define (pt row column)
  (if (or (<= column 0) (<= row column))
      1
      (+ (pt (- row 1) (- column 1)) (pt (- row 1) column))))