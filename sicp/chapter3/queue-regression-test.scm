;;; regression test for queue:
;;; Load this file in a queue implementation that provide the following procedure:
;;; make-queue, insert-queue!, delete-queue!, front-queue, empty-queue?

(load "../testframe.scm")

;; test 1
(define q1 (make-queue))

(insert-queue! q1 'a)

(assertequal? (front-queue q1) 'a)

(insert-queue! q1 'b)

(assertequal? (front-queue q1) 'a)

(delete-queue! q1)

(assertequal? (front-queue q1) 'b)

(delete-queue! q1)

(asserttrue (empty-queue? q1))

;; test2

(define q (make-queue))
(insert-queue! q 'a)
(assertequal? (front-queue q) 'a)

(insert-queue! q 'b)
(assertequal? (front-queue q) 'a)

(delete-queue! q)
(assertequal? (front-queue q) 'b)

(insert-queue! q 'c)
(assertequal? (front-queue q) 'b)

(insert-queue! q 'd)
(assertequal? (front-queue q) 'b)

(delete-queue! q)
(assertequal? (front-queue q) 'c)

(delete-queue! q)
(assertequal? (front-queue q) 'd)

(delete-queue! q)
(asserttrue (empty-queue? q))
