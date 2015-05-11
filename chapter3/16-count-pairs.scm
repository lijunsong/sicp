(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define count3 (list 1 2 3))

(define count4
  (let ((pairs (list 1 2 3)))
    (begin
      (set-car! pairs (cddr pairs))
      pairs)))

(define count5
  (let ((pairs (list 1 2 3)))
    (begin
      (set-car! pairs (cddr pairs))
      (set-car! (cdr pairs) (cddr pairs))
      pairs)))

(define count7
  (let ((pairs (list 1 2 3)))
    (begin
      (set-car! pairs (cdr pairs))
      (set-car! (cdr pairs) (cddr pairs))
      pairs)))

;;; tests begin
(load "../testframe.scm")

(assert= (count-pairs count3) 3)
(assert= (count-pairs count4) 4)
(assert= (count-pairs count5) 5)
(assert= (count-pairs count7) 7)
