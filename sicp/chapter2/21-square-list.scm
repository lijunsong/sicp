(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map (lambda (item) (square item)) items))

;;; tests begin

(load "../testframe.scm")

(let ((lst (list 2 3 4 5 6)))
  (assertequal? (square-list lst)
                (square-list-2 lst)))
