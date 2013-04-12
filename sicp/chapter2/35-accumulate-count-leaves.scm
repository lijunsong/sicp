(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;; lambda form is the same with the new-length
(define (count-leaves t)
  (accumulate (lambda (this-node next)
                (1+ next))
              0
              (enumerate-tree t)))
