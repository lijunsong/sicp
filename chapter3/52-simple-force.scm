;; cons-stream implementation

(define-syntax delay-1
  (syntax-rules ()
    ((delay-1 x)
     (lambda () x))))

(define (force-1 delayed)
  (delayed))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force-1 (cdr stream)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay-1 b)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; seq: (1 . procedure)
; sum: 1

(define y (stream-filter even? seq))
; seq: (1 . procedure)
; sum: 6
;   y: (6 . procedure)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; seq: (1 . procedure)
; sum: 15
;   z: (15 . procedure)

(stream-ref y 7)
; => 162
; seq: (1 . procedure)
; sum: 162


(display-stream z)
; seq: (1 . procedure)
;15
;180
;230
;305



