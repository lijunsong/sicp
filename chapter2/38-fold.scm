(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(load "33-accumulate.scm")

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(load "../testframe.scm")
(assert= (fold-right / 1 (list 1 2 3))
         (/ 1 (/ 2 (/ 3 1))))
(assert=  (fold-left / 1 (list 1 2 3))
          (/ (/ (/ 1 1) 2) 3))
(assertequal? (fold-right list '() (list 1 2 3))
              (list 1 (list 2 (list 3 '()))))
(assertequal? (fold-left list '() (list 1 2 3))
              (list (list (list '() 1) 2) 3))
