(define (print . exp)
  (begin
    (for-each (lambda (x) (display x))
              exp)
    (newline)))
