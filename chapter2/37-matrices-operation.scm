(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; 1. matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

;; 2. transpose
(load "36-accumulate-n.scm")
(define (transpose mat)
  (accumulate-n cons '() mat))

;; 3. matrix-*-matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col))
                cols))
         m)))

;;; tests begin
(load "../testframe.scm")
(assertequal? (dot-product '(1 2 3) '(4 5 6))
              (+ 4 10 18))
(assertequal? (transpose '((1 2 3) (4 5 6) (7 8 9)))
              '((1 4 7) (2 5 8) (3 6 9)))
(assertequal? (matrix-*-vector '((1 2 3)
                                 (4 5 6))
                               '(7 8 9))
              '(50 122))
(assertequal? (matrix-*-matrix '((1 2)
                                 (3 4)
                                 (5 6))
                               '((7 9)
                                 (8 10)))
              '((23 29)
                (53 67)
                (83 105)))
