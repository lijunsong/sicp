#lang racket

(define o-1
  (lambda (m)
    (case m
      ((add1) (lambda (x) (+ x 1)))
      ((sub1) (lambda (x) (- x 1))))))

(define (msg o m . a)
  (apply (o m) a))
; test
(msg o-1 'add1 23)

;;; object with constructors
;;; x will be initiated
(define (o-constr-1 x)
  (lambda (m)
    (case m
      ((addX) (lambda (y) (+ x y))))))
; test
(msg (o-constr-1 23) 'addX 23) ;=>46

;;; encapsulate state
;;;
(define (o-state-1 counter)
  (lambda (m)
    (case m
      ((inc) (lambda () (set! counter (+ counter 1))))
      ((dec) (lambda () (set! counter (- counter 1))))
      ((get) (lambda () counter)))))

; test
(let ((x (o-state-1 10)))
  (begin
    (msg x 'inc)
    (msg x 'get)))
;=> 11

;;; private member
(define (o-private counter)
  (let ((init counter))
    (lambda (m)
      (case m
        ((inc) (lambda () (set! init (+ init 1))))
        ((dec) (lambda () (set! init (- init 1))))
        ((get) (lambda () init))))))

; test
(let ((x (o-private 10)))
  (begin
    (msg x 'inc)
    (msg x 'get)))

;;; static member
;;; make the member common to the constructor
(define o-static 
  (let ((counter 0))
    (lambda (init)
      (begin (set! counter (+ 1 counter))
             (lambda (m)
               (case m
                 ((inc) (lambda () (set! init (+ init 1))))
                 ((dec) (lambda () (set! init (- init 1))))
                 ((get) (lambda () init))
                 ((get-counter) (lambda () counter))))))))

(let ((x (o-static 10)))
  (begin
    (msg x 'inc) 
    (display (msg x 'get)) ;11
    (msg x 'get-counter))) ;1
(let ((x (o-static 100)))
  (begin
    (msg x 'inc)
    (display (msg x 'get)) ;101
    (msg x 'get-counter))) ;2

