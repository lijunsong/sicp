(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  ;(display "\nstream-map:")
  ;(display proc) (display " ") (display s) (newline)
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

(define (display-infinite-stream s num)
  (if (> num 0)
      (begin
        (display-line (stream-car s))
        (display-infinite-stream (stream-cdr s) (- num 1)))))

(define (display-line x)
  (newline)
  (display x))

;; cons-stream implementation

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))


(define the-empty-stream '())

;; printing tracing information
(define (stream-enumerate-interval low high)
  ;(display "\nstream-enumerate-interval:") (display low) (display " ") (display high)
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

