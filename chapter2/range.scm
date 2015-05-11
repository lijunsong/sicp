;;; range function
; range will generate numbers starts from a to b-1
(define (range a b)
  (if (>= a b)
      '()
      (cons a (range (+ a 1) b))))

;;; end
