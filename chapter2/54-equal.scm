(define (new-equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        
        ((and (symbol? (car l1)) (symbol? (car l2)))
         (and (eq? (car l1) (car l2))
              (new-equal? (cdr l1) (cdr l2))))
        
        ((and (number? (car l1)) (number? (car l2)))
         (and (= (car l1) (car l2))
              (new-equal? (cdr l1) (cdr l2))))
        
        ((and (string? (car l1)) (string? (car l2)))
         (and (string=? (car l1) (car l2))
              (new-equal? (cdr l1) (cdr l2))))
        
        ((and (pair? (car l1)) (pair? (car l2)))
         (and (new-equal? (car l1) (car l2))
              (new-equal? (cdr l1) (cdr l2))))
        
        (else #f)))

;;; tests begin
(load "../testframe.scm")

;; test basic type
(let ((lst '(1 2 3 4)))
  (asserteq? (equal? lst lst)
             (new-equal? lst lst)))

(let ((lst '(a b c d)))
  (asserteq? (equal? lst lst)
             (new-equal? lst lst)))

(let ((lst '("a" "b" "c" "d")))
  (asserteq? (equal? lst lst)
             (new-equal? lst lst)))

;; test nested list

(let ((lst '(1 2 3 4 (1 2 3 4 (1 2 3 4)))))
  (asserteq? (equal? lst lst)
             (new-equal? lst lst)))
(let ((lst '(a b c (a b c (a b c)))))
  (asserteq? (equal? lst lst)
             (new-equal? lst lst)))

(let ((lst '("a" "b" ("a" "b" ("a" "b")))))
  (asserteq? (equal? lst lst)
             (new-equal? lst lst)))

