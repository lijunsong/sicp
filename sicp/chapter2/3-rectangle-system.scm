;; first, define the operation
(define (perimeter retangle)
  (+ (* 2 (length retangle))
     (* 2 (width retangle))))

(define (area retangle)
  (* (length retangle)
     (width retangle)))

;;; then the data struct
;;; retangle
;;  A  +-------------------+ D
;;     |                   |
;;     |                   |
;;  B  +-------------------+ C
(define (make-retangle A C)
  (let ((B (make-point (x-point A) (y-point C)))
        (D (make-point (x-point C) (y-point A))))
    (list A B C D)))

(define (length retangle)
  (let ((A (car retangle))
        (D (cadddr retangle)))
    (points-len A D)))

(define (width retangle)
  (let ((A (car retangle))
        (B (cadr retangle)))
    (points-len A B)))

;;; point operation
;;; get length between two points
(define (points-len p1 p2)
  (let ((len1 (abs (- (x-point p1) (x-point p2))))
        (len2 (abs (- (y-point p1) (y-point p2)))))
    (sqrt (+ (* len1 len1)
             (* len2 len2)))))

;;; point data structure
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))


;;; tests begin
(load "../testframe.scm")

(let* ((A (make-point 1 3))
       (C (make-point 8 1))
       (ret (make-retangle A C)))
  (begin
    (assert= 18 (perimeter ret))
    (assert= 14 (area ret))))
