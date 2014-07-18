;; stack for machine
(load "../testframe.scm")

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max max-depth current-depth)))
    (define (pop)
      (if (null? s)
          (error 'make-stack "Empty Stack -- POP")
          (let ((x (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            x)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch msg)
      (cond ((eq? msg 'push) push)
            ((eq? msg 'pop) (pop))
            ((eq? msg 'initialize) (initialize))
            ((eq? msg 'print-statistics) (print-statistics))
            (else
             (error 'make-stack "Unknown request -- STACK" msg))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack v)
  ((stack 'push) v))

;; tests begin
(let ((s (make-stack)))
  (begin
    (push s 12)
    (push s 'a)
    (asserteq? (pop s) 'a)
    (assert= (pop s) 12)
    (assert/exn (pop s) "Empty")
    (push s 13)
    (s 'initialize)
    (assert/exn (pop s) "Empty")))