(define (last-pair lst)
  (cond ((null? (cdr lst)) lst)
        (else (last-pair (cdr lst)))))


;;; tests begin
(load "../testframe.scm")

(assertequal? (last-pair '(1)) '(1))
(assertequal? (last-pair '(a b c d)) '(d))

