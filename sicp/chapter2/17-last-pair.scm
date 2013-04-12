(define (last-pair lst)
  (cond ((null? (cdr lst)) lst)
        (else (last-pair (cdr lst)))))
