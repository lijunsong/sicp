(define (same-parity x . z)
  (define (same-parity-rec rest)
    (cond ((null? rest) rest)
          ((and (even? x) (even? (first rest)))
           (cons (first rest) (same-parity-rec (cdr rest))))
          ((even? x) (same-parity-rec (cdr rest)))
          ((and (odd? x) (odd? (first rest)))
           (cons (first rest) (same-parity-rec (cdr rest))))
          ((odd? x) (same-parity-rec (cdr rest)))))
  (cons x (same-parity-rec z)))

;;; tests begin

(load "../testframe.scm")

(assertequal? (same-parity 1 2 3 4 5 6 7)
              (list 1 3 5 7))

(assertequal? (same-parity 2 3 4 5 6 7)
              (list 2 4 6))
