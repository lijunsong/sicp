(define (make-accumulator sum)
  (lambda (add)
    (set! sum (+ add sum))
    sum))

