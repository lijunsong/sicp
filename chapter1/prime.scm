(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next num)
    (cond ((even? num) (+ num 1))
          (else (+ num 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (not (= n 1)) (= (smallest-divisor n) n)))
