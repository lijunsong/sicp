(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map proc subtree)
             (proc subtree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;;; tests begin
(load "../testframe.scm")

(define tree '(1 2 (3 4 5) (6 (7 (8 (9))))))

(assertequal? (square-tree tree) '(1 4 (9 16 25) (36 (49 (64 (81))))))
