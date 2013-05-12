;;; count-leaves in section 2.2.2
(load "33-accumulate.scm")
(define (count-leaves/accumulate t)
  (accumulate + 0 (map (lambda (sub)
                         (if (pair? sub)
                             (count-leaves/accumulate sub)
                             1))
                       t)))

;;; other solutions
(define (count-leaves/correct x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves/correct (car x))
                 (count-leaves/correct (cdr x))))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves/flat t) ; sort of the same with the length procedure
  (accumulate (lambda (this-node next)
                (1+ next))
              0
              (enumerate-tree t)))

;;; tests begin
(load "../testframe.scm")

(let ((l0 '())
      (l1 '(1))
      (l2 '(1 3))
      (l3 '(1 (2 (3 (4)))))
      (l4 '((((4) 3) 2) 1)))
  (assert= (count-leaves/correct l0) (count-leaves/accumulate l0))
  (assert= (count-leaves/correct l0) (count-leaves/flat l0))

  (assert= (count-leaves/correct l1) (count-leaves/accumulate l1))
  (assert= (count-leaves/correct l1) (count-leaves/flat l1))

  (assert= (count-leaves/correct l2) (count-leaves/accumulate l2))
  (assert= (count-leaves/correct l2) (count-leaves/flat l2))

  (assert= (count-leaves/correct l3) (count-leaves/accumulate l3))
  (assert= (count-leaves/correct l3) (count-leaves/flat l3))

  (assert= (count-leaves/correct l4) (count-leaves/accumulate l4))
  (assert= (count-leaves/correct l4) (count-leaves/flat l4)))
