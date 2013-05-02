(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; 1
(fixed-point (lambda (x)
               (/ (log 1000) (log x)))
             2.0)

; average damping
(fixed-point (lambda (x)
               (/ (+ x (/ (log 1000) (log x)))
                  2))
             2.0)
