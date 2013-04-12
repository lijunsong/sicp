;;; check the condition that an interval spans zero in constructor
(define (make-interval a b)
  (if (and (< (min a b) 0)
           (> (max a b) 0))
      (error 'make-interval "interval should not span zero!")
      (cons a b)))
