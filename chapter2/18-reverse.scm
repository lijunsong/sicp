(define (reverse lst)
  (cond ((null? lst) lst)
        (else
         (append (reverse (cdr lst)) (list (car lst))))))

(define (new-reverse lst)
  (define (iter l reversed)
    (if (null? l)
        reversed
        (iter (cdr l)
              (cons (car l) reversed))))
  (iter lst '()))
;;; tests begin

(load "../testframe.scm")

(assertequal? (reverse '(1 2 3 4))
              '(4 3 2 1))

(assertequal? (new-reverse '(1 2 3 4))
              '(4 3 2 1))
