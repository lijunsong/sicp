;; regression test for Set, sicp 2.3.3

(load "../testframe.scm")

; element-of-set?
(asserttrue (element-of-set? 1 '(1 2 3)))

(asserttrue (element-of-set? 1 '(2 1 3)))

(asserttrue (element-of-set? 1 '(2 3 1)))

; adjoin-set
(assertequal? (sort (adjoin-set 1 '(2 3 4)) <)
              '(1 2 3 4))

(assertequal? (sort (adjoin-set 1 '(1 2 3)) <)
              '(1 2 3))

; intersection-set
(assertequal? (sort (intersection-set '(1 2 3) '(4 5 6)) <)
              '())

(assertequal? (sort (intersection-set '(1 2 3) '(2 3 4)) <)
              '(2 3))

(assertequal? (sort (intersection-set '(1 2 3) '(1 2 3 4)) <)
              '(1 2 3))

(assertequal? (sort (intersection-set '(1 2 3 4) '(1 2)) <)
              '(1 2))

; union-set
(assertequal? (sort (union-set '(1 2 3) '(3 4 5)) <)
              '(1 2 3 4 5))

(assertequal? (sort (union-set '(1 2 3) '(1 2 3 4)) <)
              '(1 2 3 4))

(assertequal? (sort (union-set '(1 2 3 4) '(1 2 3)) <)
              '(1 2 3 4))

(assertequal? (sort (union-set '(1 2 3) '(4 5 6)) <)
              '(1 2 3 4 5 6))
