(load "huffman-tree.scm")

(load "67-decode-sample.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;;; 1.
;;; recursively encode the symbol
(define (encode-symbol sym tree)
  (if (leaf? tree)
      '()
      (let ((code (encoding sym tree)))
        (if (= code 1)
            (cons code
                  (encode-symbol sym
                                 (right-branch tree)))
            (cons code
                  (encode-symbol sym
                                 (left-branch tree)))))))
;;; 2. 
;;; decide the branch
(define (encoding sym tree)
  (if (not (element-of-set? sym (symbols tree)))
      (error 'encode-symbol "symbol NOT FOUND" sym " in " tree)
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (element-of-set? sym (symbols left))
            0
            1))))

;;; tests begin
(load "../testframe.scm")

(assertequal? (encode '(a d a b b c a) sample-tree)
              sample-message)

(assert/exn (encode '(a e a) sample-tree) "NOT FOUND")
