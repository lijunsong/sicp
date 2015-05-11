(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((not (pair? (car lst))) ; atom?
         (append (deep-reverse (cdr lst)) (list (car lst))))
        (else  ; else
         (append (deep-reverse (cdr lst))
                 (list (deep-reverse (car lst)))))))


;;; tests begin
(load "../testframe.scm")

(assertequal? (deep-reverse '((1 2 3 (4 5)) (6 7)))
              '((7 6) ((5 4) 3 2 1)))

(let ((x (list (list 1 2) (list 3 4))))
  (assertequal? (deep-reverse x)
                '((4 3) (2 1))))

