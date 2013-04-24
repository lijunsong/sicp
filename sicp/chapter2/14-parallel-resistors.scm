(load "12-make-center-percent.scm")

(load-option 'format)

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;; Lem is right
(define r1 (make-center-percent 6.8 0.1))
(define r2 (make-center-percent 4.7 0.05))

(let ((result-par1 (par1 r1 r2))
      (result-par2 (par2 r1 r2)))
  (format #t
          "~%par1: ~a[~5a(~5a)]"
          result-par1 (center result-par1) (percent result-par1))
  (format #t
          "~%par2: ~a[~5a(~5a)]"
          result-par2 (center result-par2) (percent result-par2)))

;; par1: (2.201031010873943 . 3.4873689182805854)[2.844(.2261)]
;; par2: (2.581558809636278 . 2.97332259363673)[2.777(.0705)]

;; test a and b
(define A (make-center-percent 10 0.01))
(define B (make-center-percent 15 0.05))

(format #t "~%A/A's percent:~a" (percent (div-interval a a)))
; A/A's percent:1.9998000199979854e-2
; or 0.02 <- 0.01 + 0.01

(format #t "~%A/B's percent:~a" (percent (div-interval a b)))
; A/B's percent:.05997001499250375
; or 0.06 <- 0.05 + 0.01

