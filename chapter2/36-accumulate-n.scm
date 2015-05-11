(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (lst)
                                       (car lst))
                                     seqs))
            (accumulate-n op init (map (lambda (lst)
                                         (cdr lst))
                                       seqs)))))

;;; tests begin
(load "../testframe.scm")
(assertequal?
 (accumulate-n +
               0
               '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
 '(22 26 30))
