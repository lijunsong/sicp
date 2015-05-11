(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (test-fermat n)
  (define (is-fermat-fine? a)
    (= a (expmod a n n)))
  (define (test-each-num-below x)
    (cond ((= x 2) #t)
          ((is-fermat-fine? x)
           (test-each-num-below (- x 1)))
          (else #f)))
  (test-each-num-below (- n 1)))

;;; tests begin

(load "../testframe.scm")
;; test-fermat will return #t if n is decided being prime using fermat method
;; #f otherwise
(asserteq? #t (test-fermat 199))
(asserteq? #t (test-fermat 1999))
(asserteq? #f (test-fermat 19999))

;; carmichael number!
(asserteq? #t (test-fermat 561))
(asserteq? #t (test-fermat 1105))
(asserteq? #t (test-fermat 1729))
(asserteq? #t (test-fermat 2465))
(asserteq? #t (test-fermat 2821))
(asserteq? #t (test-fermat 6601))

;; but the prime? procedure can decide the primality
(define (find-divisor n test-divisor)
  (define (divides? a b)
  (= (remainder b a) 0))  
  (define (next num)
    (cond ((even? num) (+ num 1))
          (else (+ num 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))  
  (= n (smallest-divisor n)))

(asserteq? #f (prime? 561))
(asserteq? #f (prime? 1105))
(asserteq? #f (prime? 1729))
(asserteq? #f (prime? 2465))
(asserteq? #f (prime? 2821))
(asserteq? #f (prime? 6601))

