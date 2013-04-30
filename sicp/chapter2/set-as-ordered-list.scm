;;; represent the set as list in increasing order

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((> x (car set)) #f)
        ((= x (car set)) #t)
        (else
         (element-of-set? x (cdr set)))))

; adjoin-set
(define (adjoin-set x set)
  (cond ((null? set) '())
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set))))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))))

; end of adjoin-set

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2))
         (cons (car set1)
               (intersection-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (intersection-set (cdr set1) set2))
        ((> (car set1) (car set2))
         (intersection-set set1 (cdr set2)))))

; union-set

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2)
               (union-set set1 (cdr set2))))))


(load "set-regression-tests.scm")
