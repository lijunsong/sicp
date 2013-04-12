(define matrix '((1 2 3 4)
                 (4 5 6 6)
                 (6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (map * v row))
       m))

; (matrix-*-vector matrix '(1 2 3 4))
(load "36-accumulate-n.scm")
(define (transpose mat)
  (accumulate-n cons '() mat))

; (transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (accumulate + 0 (map * m-row n-col)))
                cols))
         m)))

(matrix-*-matrix '((1 1)
                   (2 0))
                 '((0 2 3)
                   (1 1 2)))
