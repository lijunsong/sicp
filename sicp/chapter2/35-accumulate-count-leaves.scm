;; count-leaves in section 2.2.2
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;; lambda form is the same with the new-length
(define (count-leaves t)
  (accumulate (lambda (this-node next)
                (1+ next))
              0
              (enumerate-tree t)))

(define (count-leaves t)
  (accumulate (lambda (this-node next)
                (+ 1 next)) 0 (map count-leaves t)))
