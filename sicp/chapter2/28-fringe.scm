(define x '((1 2) (3 4)))


;; always keep current lst as a non-nested list
(define (fringe lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (fringe (car lst))
                 (fringe (cdr lst))))
        (else
         (append (list (car lst))
                 (fringe (cdr lst))))))
