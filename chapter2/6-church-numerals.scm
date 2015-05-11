(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; so, what is one?
(define one
  (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (f x)))
; and what is two?
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; => (lambda (f) (lambda (x) (f (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

;;; TODO: check if it is right
(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
