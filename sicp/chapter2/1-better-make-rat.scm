;;; a better version of make-rat that can handle sign: normalize the
;;; sign that if the rational number is positive, both the numerator
;;; and denominator are positive, and if the rational number is
;;; negative, only the numerator is negative.

(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (let* ((n-and-d
          (cond ((and (< n 0) (< d 0)) (cons (- 0  n) (- 0  d)))
                ((< d 0) (cons (- 0 n) (- 0 d)))
                (else (cons n d))))
         (g (gcd n d)))
    (cons (/ (car n-and-d) g)
          (/ (cdr n-and-d) g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

