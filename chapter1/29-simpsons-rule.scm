(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpson-rule f a b n)
  (let ((h (/ (- b a) n)))  
    (define (next t)
      (+ h h t))
    (* (/ h 3.0)
       (+ (f a)
          (f b)
          (* 4.0 (sum f (+ a h) next (+ a (* h (- n 1)))))
          (* 2.0 (sum f (+ a h h) next (+ a (* h (- n 2)))))))))

;;; tests begin
(define (cube x)
  (* x x x))

(simpson-rule cube 0 1 100) ; ;Value: .25

(simpson-rule cube 0 1 1000) ; ;Value: .25
