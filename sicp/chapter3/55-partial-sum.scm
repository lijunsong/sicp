(load "stream.scm")
(load "50-stream-map.scm")
(load "integer-stream.scm")

(define (partial-sums s)
  (define partial (cons-stream (stream-car s)
                               (add-streams partial (stream-cdr s))))
  partial)
