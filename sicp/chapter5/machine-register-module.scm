;; make-register
(load "../testframe.scm")

(define (make-register)
  (let ((content '*unassigned*))
    (define (set-content v)
      (set! content v))
    (define (dispatch msg)
      (cond ((eq? msg 'get) content)
            ((eq? msg 'set) set-content)
            (else (error "UNKNOWN COMMAND -- make-register" message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register v)
  ((register 'set) v))


;; tests begin
(let ((r (make-register)))
  (begin
    (asserteq? (get-contents r) '*unassigned*)
    (set-contents! r 12)
    (asserteq? (get-contents r) 12)))
