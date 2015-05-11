(define (cube-root x)
  (define (cube-root-iter last-guess guess x)
    (if (good-enough? last-guess guess)
        guess
        (cube-root-iter guess (improve guess x)
                        x)))

  (define (improve guess x)
    (average guess (/ (+ (/ x (* guess guess)) (* 2 guess))
                      3)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? last-guess guess)
    (< (/ (abs (- last-guess guess)) guess) 0.001))
  
  (cube-root-iter 0 1.0 x))
