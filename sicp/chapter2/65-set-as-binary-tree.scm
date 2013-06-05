(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))


(define (list->tree elements)
  
  (car (partial-tree (sort elements <) (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;; union-set
(define (union-set set1 set2)
  (define (union-ordered-list lst1 lst2)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          ((= (car lst1) (car lst2))
           (cons (car lst1)
                 (union-ordered-list (cdr lst1) (cdr lst2))))
          ((> (car lst1) (car lst2))
           (cons (car lst2)
                 (union-ordered-list lst1 (cdr lst2))))
          (else
           (cons (car lst1)
                 (union-ordered-list (cdr lst1) lst2)))))  
  (let ((set1-list (tree->list set1))
        (set2-list (tree->list set2)))
    (list->tree (union-ordered-list set1-list set2-list))))

;;; intersection-set
(define (intersection-set set1 set2)
  (define (intersection-ordered-list lst1 lst2)
    (cond ((or (null? lst1) (null? lst2)) '())
          ((= (car lst1) (car lst2))
           (cons (car lst1)
                 (intersection-ordered-list (cdr lst1) (cdr lst2))))
          ((> (car lst1) (car lst2))
           (intersection-ordered-list lst1 (cdr lst2)))
          (else
           (intersection-ordered-list (cdr lst1) lst2))))  
  (let ((set1-list (tree->list set1))
        (set2-list (tree->list set2)))
    (list->tree (intersection-ordered-list set1-list set2-list))))

;; regression test for Set AS BINARY TREE

(load "../testframe.scm")

; element-of-set?
(asserttrue (element-of-set? 1 (list->tree '(1 2 3))))

(asserttrue (element-of-set? 1 (list->tree '(2 1 3))))

(asserttrue (element-of-set? 1 (list->tree '(2 3 1))))

; adjoin-set
(assertequal? (tree->list (adjoin-set 1 (list->tree '(2 3 4))))
              '(1 2 3 4))

(assertequal? (tree->list (adjoin-set 1 (list->tree '(1 2 3))))
              '(1 2 3))

; intersection-set
(assertequal? (intersection-set (list->tree '(1 2 3)) (list->tree '(4 5 6)))
              '())

(assertequal? (tree->list (intersection-set (list->tree '(1 2 3)) (list->tree '(2 3 4))))
              '(2 3))

(assertequal? (tree->list (intersection-set (list->tree '(1 2 3)) (list->tree '(1 2 3 4))))
              '(1 2 3))

(assertequal? (tree->list (intersection-set (list->tree '(1 2 3 4)) (list->tree '(1 2))))
              '(1 2))

; union-set
(assertequal? (tree->list (union-set (list->tree '(1 2 3)) (list->tree '(3 4 5))))
              '(1 2 3 4 5))

(assertequal? (tree->list (union-set (list->tree '(1 2 3)) (list->tree '(1 2 3 4))))
              '(1 2 3 4))

(assertequal? (tree->list (union-set (list->tree '(1 2 3 4)) (list->tree '(1 2 3))))
              '(1 2 3 4))

(assertequal? (tree->list (union-set (list->tree '(1 2 3)) (list->tree '(4 5 6))))
              '(1 2 3 4 5 6))
