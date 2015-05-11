(define (ineffective-sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  
  (sqrt-iter 1.0 x))

;; new-sqrt
(define (new-sqrt x)
  (define (sqrt-iter last-guess guess x)
    (if (good-enough? last-guess guess)
        guess
        (sqrt-iter guess (improve guess x)
                   x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? last-guess guess)
    (< (/ (abs (- last-guess guess)) guess) 0.001))
  
  (sqrt-iter 0 1.0 x))
