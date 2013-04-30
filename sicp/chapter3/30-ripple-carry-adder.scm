(define (ripple-carry-adder as bs ss c)
  (define (sequence-adder a b s c-out)
    (if (not (null? (cdr a)))
        (let ((new-in (make-wire)))
          (begin
            (full-adder (car a) (car b) new-in (car s) c-out)            
            (sequence-adder (cdr a) (cdr b) (cdr s) new-in)))
        (full-adder (car a) (car b) 0 (car s) c-out)))
  (sequence-adder as bs ss c))
