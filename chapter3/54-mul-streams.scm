(load "integer-stream.scm")

;; mul-streams

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; factorials
(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

;;; tests begin
(load "../testframe.scm")

(assert= (stream-ref factorials 0) 1) ;1!
(assert= (stream-ref factorials 1) 2) ;2!
(assert= (stream-ref factorials 2) 6) ;3!
(assert= (stream-ref factorials 3) 24) ;4!
(assert= (stream-ref factorials 4) 120) ;5!




