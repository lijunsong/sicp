(load "33-accumulate.scm")

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (define (divide? a b)
    (= (remainder a b) 0))
  (define prime-rec?
    (let ((maxone (sqrt n)))
      (lambda (x)
        (cond ((> x maxone) #t)
              ((divide? n x) #f)
              (else (prime-rec? (+ 1 x)))))))
  (prime-rec? 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

;;; unique-pairs
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (simplified-prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
