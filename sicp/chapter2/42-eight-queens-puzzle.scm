(load "40-unique-pairs.scm")

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; 1.
(define empty-board '())

;; 2.
(define (adjoin-position row col positions)
  (append positions (list row)))


;; check row and diagonal
;; 判断位于 (row1, col1) 是否和 (row2, col2) 在同一条斜线上
;; row1 + b = col1
;; row2 + b = col2
;; col1 - row1 == col2 - row2
;; 或者 col1 + ro1 == col2 + row2
;; 3. safe?
(define (safe? k positions)
  (define (safe-pos? row1 col1 row2 col2)
    (cond ((= row1 row2) #f)
          ((= (- col1 row1) (- col2 row2)) #f)
          ((= (+ col1 row1) (+ col2 row2)) #f)
          (else #t)))
  (define (safe/rec? row col rows col1)
    (cond ((null? (cdr rows)) #t)
          ((safe-pos? row col (car rows) col1)
           (safe/rec? row col (cdr rows) (+ 1 col1)))
          (else #f)))
  (let ((c k)
        (r (list-ref positions (- k 1))))
    (safe/rec? r c positions 1)))
;; ends-3
