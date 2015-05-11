(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((maybe-nontrivial (remainder (square (expmod base (/ exp 2) m)) m)))
           (cond ((and (not (= base 1))
                       (not (= base (- m 1)))
                       (= maybe-nontrivial 1)) 0)
                 (else maybe-nontrivial))))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (miller-rabin n)
  (define (miller-rabin-test a)
    (let ((ans (expmod a (- n 1) n)))
      (if (or (= 0 ans) (= 1 ans))))
    (if (= 0 (expmod a (- n 1) n))
        #f ;not prime
        #t))
  (miller-rabin-test (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin n) (miller-rabin-prime? n (- times 1)))
        (else #f)))

(define (test-miller-rabin n)
  (miller-rabin-prime? n (ceiling (sqrt n))))

;;; tests begin

(load "../testframe.scm")
;; test-miller-rabin will return #t if n is decided being prime using Miller Rabin method
;; #f otherwise
(asserteq? #t (test-miller-rabin 199))
(asserteq? #t (test-miller-rabin 1999))
(asserteq? #f (test-miller-rabin 19999))

;; carmichael number!
(asserteq? #f (test-miller-rabin 561))
(asserteq? #f (test-miller-rabin 1105))
(asserteq? #f (test-miller-rabin 1729))
(asserteq? #f (test-miller-rabin 2465))
(asserteq? #f (test-miller-rabin 2821))
(asserteq? #f (test-miller-rabin 6601))

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

