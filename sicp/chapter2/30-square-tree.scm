(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (* subtree subtree)))
       tree))

(define (square-tree-iter tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else
         (cons (square-tree-iter (car tree))
               (square-tree-iter (cdr tree))))))

(define tree '(1 2 (3 4 5) (6 (7 (8 (9))))))
