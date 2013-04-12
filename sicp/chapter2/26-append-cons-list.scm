(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
;Value 12: (1 2 3 4 5 6)
; do not need additional box

(cons x y)
;Value 13: ((1 2 3) 4 5 6)
; create a new box

(list x y)
;Value 14: ((1 2 3) (4 5 6))
; create two new boxes.
