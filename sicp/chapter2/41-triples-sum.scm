;;; exercise 41

(load "33-accumulate.scm")
(load "40-unique-pairs.scm")
(load "range.scm")

;; 1. filter unique triples
(define (unique-triples-1 n)
  (define (unique-triple? triple)
    (let ((a (car triple))
          (b (cadr triple))
          (c (caddr triple)))
      (if (or (= a b) (= a c) (= b c))
          #f
          #t)))
  (filter unique-triple?
          (flatmap
           (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 n)))
                      (enumerate-interval 1 n)))
           (enumerate-interval 1 n))))
; ends-1

;; 2. use permuatation
(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-triples-2 n)
  (flatmap permutations
           (flatmap (lambda (i) ; generate (i, j, k) so that i < j < k
                      (flatmap (lambda (k)
                                 (map (lambda (j)
                                        (list i j k))
                                      (enumerate-interval (+ 1 i) (- k 1))))
                               (enumerate-interval (+ 2 i) n)))
                    (enumerate-interval 1 n))))
; ends-2

;;; 3 
;;; find all ordered triples of distinct positive integers i, j, and k
;;; less than or equal to a given integer n that sum to a given
;;; integer s. 
(define (ordered-triples-sum n s)
  (define (triple-sum t)
    (let ((a (car t))
          (b (cadr t))
          (c (caddr t)))
      (cons (+ a b c) t)))
  (map (lambda (t-sum) (cdr t-sum))
       (filter (lambda (x) (= s (car x)))
               (map triple-sum
                    (unique-triples-1 n)))))
; ends-3
