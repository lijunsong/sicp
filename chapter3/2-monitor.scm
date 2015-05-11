(define make-monitored
  (let ((how-many 0))
    (lambda (f)
      (lambda (arg)
        (cond ((eq? arg 'how-many-calls?)
               how-many)
              ((eq? arg 'reset-count)
               (set! how-many 0))
              (else
               (begin
                 (set! how-many (+ 1 how-many))
                 (f arg))))))))

(define s (make-monitored sqrt))

