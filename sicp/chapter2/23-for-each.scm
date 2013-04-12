(define (for-each proc lst)
  (cond ((null? lst) true)
        (else
         (begin
           (proc (car lst))
           (for-each proc (cdr lst))))))
