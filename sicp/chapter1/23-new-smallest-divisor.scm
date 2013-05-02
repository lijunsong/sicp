(load "22-search-for-primes.scm")

(define (smallest-divisor n)
  (find-divisor n 2))

;; new one with next procedure
(define (find-divisor n test-divisor)
  (define (next num)
    (cond ((even? num) (+ num 1))
          (else (+ num 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
;; end

(define (divides? a b)
  (= (remainder b a) 0))

;; begin search
(search)
