(load "stream.scm")

(define (sqrt-improve guess x)
  (define (average x y) (* 0.5 (+ x y)))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (display "(sqrt-stream ") (display x) (display ")") (newline)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

(define (t1 x)
  (cons-stream 1
               (stream-map (lambda (g)
                             (begin
                               (display g)
                               (+ g x)))
                           (t1 x))))
