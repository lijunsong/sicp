(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(let ((v '(1 2 3 4)))
  (let ((w (mystery v)))
    w))
