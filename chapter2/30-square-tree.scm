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

;;; tests begin
(load "../testframe.scm")

(define tree '(1 2 (3 4 5) (6 (7 (8 (9))))))

(assertequal? (square-tree tree) '(1 4 (9 16 25) (36 (49 (64 (81))))))
(assertequal? (square-tree-iter tree) (square-tree tree))
