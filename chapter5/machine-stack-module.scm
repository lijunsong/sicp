;; stack for machine
(load "../testframe.scm")

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error 'make-stack "Empty Stack -- POP")
          (let ((x (car s)))
            (set! s (cdr s))
            x)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch msg)
      (cond ((eq? msg 'push) push)
            ((eq? msg 'pop) (pop))
            ((eq? msg 'initialize) (initialize))
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
