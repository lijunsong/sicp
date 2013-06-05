(load "stream.scm")

(define (stream-map proc . argstreams)
  (if (null? (stream-car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

;; compare with

(define (my-map proc . args)
  (define (map-list proc items)
    (if (null? items)
        '()
        (cons (proc (car items))
              (map-list proc (cdr items)))))
  (if (null? (car args))
      '()
      (cons
       (apply proc (map-list car args))
       (apply my-map (cons proc (map-list cdr args))))))


;;; tests begin
; tests will be taken from section 3.5.2

(define (add-streams s1 s2)
  (stream-map + s1 s2))

