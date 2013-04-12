(define (reverse lst)
  (cond ((null? lst) lst)
        (else
         (append (reverse (cdr lst)) (list (car lst))))))
