;; using append can always keep current lst as a list of atom
(define (fringe lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (fringe (car lst))
                 (fringe (cdr lst))))
        (else
         (append (list (car lst))
                 (fringe (cdr lst))))))

;;; tests begin
(load "../testframe.scm")

(let ((x '((1 2) (3)))
      (y '(1 (2 3)))
      (z '((1 2) 3)))
  (assertequal? (fringe x)
                '(1 2 3))
  (assertequal? (fringe y)
                '(1 2 3))
  (assertequal? (fringe z)
                '(1 2 3)))
