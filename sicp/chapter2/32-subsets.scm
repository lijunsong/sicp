(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;; tests begin

(load "../testframe.scm")

;; fixme: assertequal? is not a right assertion for set
(assertequal? (subsets '(1 2 3))
              '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
