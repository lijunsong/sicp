(define x (list (list 1 2) (list 3 4)))

;;; refer to "the little schemer"
;;; TODO: fix this:
(define (deep-reverse lst)
  (cond ((null? (cdr lst)) lst)
        ((not (pair? (car lst)))
         (append (deep-reverse (cdr lst))
                 (list (car lst))))
        (else
         (list (deep-reverse (cdr lst))
               (deep-reverse (car lst))))))

(deep-reverse '((1 2 3 (4 5)) (6 7)))
(deep-reverse '(1 2 3 4))
