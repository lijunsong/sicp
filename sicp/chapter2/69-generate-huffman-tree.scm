(load "huffman-tree.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge
       (adjoin-set (make-code-tree (car pairs)
                                   (cadr pairs))
                   (cddr pairs)))))


;; (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
;; =>
;; ((leaf a 8)
;;  ((((leaf h 1)
;;     (leaf g 1) (h g) 2)
;;    ((leaf f 1)
;;     (leaf e 1) (f e) 2) (h g f e) 4)
;;   (((leaf d 1) (leaf c 1) (d c) 2)
;;    (leaf b 3) (d c b) 5) (h g f e d c b) 9)
;;  (a h g f e d c b) 17)
