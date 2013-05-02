(define (repeated f times)
  (lambda (x)
    (cond ((= times 0) x)
          (else
           ((repeated f (- times 1))
            (f x))))))

(define (repeated/iter f times)
  (define (iter current-time result)
    (cond ((> current-time times) result)
          (else
           (iter (+ current-time 1) (f result)))))
  (lambda (x)
    (iter 1 x)))

; with compose as glue
(define (repeated/compose f times)
  (cond ((= times 0) (lambda (x) x))
        (else
         (lambda (x)
           (f ((repeated/compose f (- times 1)) x))))))

;;; tests begin

(assert= ((repeated square 0) 5) 5)
(assert= ((repeated/iter square 0) 5) 5)
(assert= ((repeated/compose square 0) 5) 5)

(assert= ((repeated square 1) 5) 25)
(assert= ((repeated/iter square 1) 5) 25)
(assert= ((repeated/compose square 1) 5) 25)

(assert= ((repeated square 2) 5) 625)
(assert= ((repeated square 2) 5)
         ((repeated/iter square 2) 5))
(assert= ((repeated/iter square 2) 5)
         ((repeated/compose square 2) 5))

(let ((v1-raise-power-to-2^n (lambda (n)
                            ((repeated/iter square n) 2)))
      (v2-raise-power-to-2^n (lambda (n)
                                    ((repeated square n) 2)))
      (v3-raise-power-to-2^n (lambda (n)
                                    ((repeated/compose square n) 2)))      )
  (assert= (v1-raise-power-to-2^n 4) (* 1024 64))
  (assert= (v2-raise-power-to-2^n 4) (* 1024 64))
  (assert= (v3-raise-power-to-2^n 4) (* 1024 64))  )

(let ((v1+n (lambda (n)
            (repeated (lambda (x) (+ x 1)) n)))
      (v2+n (lambda (n)
                    (repeated/iter (lambda (x) (+ x 1)) n)))
      (v3+n (lambda (n)
                    (repeated/compose (lambda (x) (+ x 1)) n))))
  (assert= ((v1+n 20) 1) 21)
  (assert= ((v2+n 20) 1) 21)
  (assert= ((v3+n 20) 1) 21))
