(define rand
  (let ((x random-init))
    (lambda (cm)
      (cond ((eq? cm 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? cm 'reset)
             (lambda (new-value)
               (begin
                 (set! x new-value)
                 x)))
            (else (error "command error -- RAND"))))))
