(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (new-map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))

(define (new-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (new-length sequence)
  (accumulate (lambda (x y)
                (1+ y))
              0 sequence))


;;; tests begin

(load "../testframe.scm")

(assertequal? (new-map (lambda (x) (+ x 1))
                       '(1 2 3 4 5))
              (map (lambda (x) (+ x 1))
                   '(1 2 3 4 5)))

(assertequal? (new-append '(1 2 3 4) '(5 6 7))
              (append '(1 2 3 4) '(5 6 7)))

(assertequal? (new-length '(1 2 (3 4)))
              (length '(1 2 (3 4))))
