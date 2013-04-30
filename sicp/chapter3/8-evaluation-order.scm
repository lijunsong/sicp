(define f
  (let ((x 1))
    (lambda (z)
      (set! x (* x z))
      x)))

;; evalution from right to left, which is the order of the scheme
;; 1. x = (f 0)
;; 2. y = (f 1)
;; 3. z = x + y

(+ (f 1) (f 0))
; => 0

; reload again, rename it to f1
(define f1
  (let ((x 1))
    (lambda (z)
      (set! x (* x z))
      x)))

;; 1. x = (f 1)
;; 2. y = (f 0)
;; 3. z = x + y
(+ (f1 0) (f1 1))
; => 1
