(load "38-fold.scm")

; fold-right
(define (reverse-1 sequence)
  (fold-right (lambda (x y)
                (append y (list x))) '() sequence))

; fold-left
(define (reverse-2 sequence)
  (fold-left (lambda (x y)
               (append (list y) x)) '() sequence))


;;; tests begin
(load "../testframe.scm")
(let ((l '(1 2 3 4 5))
      (l1 '(1)))
  (assertequal? (reverse-1 l) '(5 4 3 2 1))
  (assertequal? (reverse-2 l) '(5 4 3 2 1))
  (assertequal? (reverse-1 l1) '(1))
  (assertequal? (reverse-2 l1) '(1)))
