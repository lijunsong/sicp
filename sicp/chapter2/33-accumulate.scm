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

;;; 使用 accumulate 的思路，将 (lambda (x y) ...) 中的 y 用 initial 来
;;; 代替，得到最后一次使用 lambda 式的情景，然后往前推导。
(define (new-length sequence)
  (accumulate (lambda (x y)
                (1+ y))
              0 sequence))
