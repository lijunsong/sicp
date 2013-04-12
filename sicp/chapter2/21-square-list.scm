(define (square-list items)
  (if (null? items)
      nil
      (cons (square (first items))
            (square-list (rest items)))))

(define (square-list-2 items)
  (map (lambda (item) (square item)) items))
