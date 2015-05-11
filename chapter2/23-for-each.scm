(define (for-each proc lst)
  (cond ((null? lst) true)
        (else
         (begin
           (proc (car lst))
           (for-each proc (cdr lst))))))

;;; tests begin
(load "../testframe.scm")

(assert-output-equal
 (for-each (lambda (x) (newline) (display x))
           (list 57 321 88))
 "
57
321
88")
