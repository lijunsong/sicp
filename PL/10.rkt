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

;;; dynamic dispatch
(define (mt)
  (lambda (m)
    (case m
      ((add) (lambda () 0)))))
(define (node v l r)
  (lambda (m)
    (case m
      ((add) (lambda () (+ v 
                           (msg l 'add)
                           (msg r 'add)))))))
(define a-tree
  (node 10
        (node 15 (mt) (mt))
        (node 15 (mt) (mt))))
;;; method selection is built in
(msg a-tree 'add) ;=> (+ 10 15 15)

;;; construct with parent maker
(define (mt/size parent-maker)
  (let ((parent (parent-maker)))
    (lambda (m)
      (case m
        ((size) (lambda () 0))
        (else (parent m))))))

(define (node/size parent-maker v l r)
  (let ((parent (parent-maker v l r)))
    (lambda (m)
      (case m
        ((size) (lambda () (+ 1
                              (msg l 'size)
                              (msg r 'size))))
        (else (parent m))))))
;; test the parent maker
(define a-tree/size
  (node/size node 5
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 10 (mt/size mt) (mt/size mt))))
(msg a-tree/size 'size)
(msg a-tree/size 'add)

;; test the parent maker with different type of node
;(define a-tree-2/size
;  (node/size node 5
;             (node/size mt 5 (mt/size mt) (mt/size mt))
;             (node/size mt 10 (mt/size mt) (mt/size mt))))
;(msg a-tree-2/size 'size)

;;;;;;;;;;;;;;; define the binary tree with self-reference
(define (msg/self o m . a)
  (apply (o m) o a))

(define (mt/self)
  (lambda (m)
    (case m
      ((add) (lambda (self) 0)))))
; TODO: should this be (msg/self self 'get-l 'add)
; to explicitly get l and r from self?
(define (node/self v l r)
  (lambda (m)
    (case m
      ((add) (lambda (self) (+ v 
                               (msg/self l 'add)
                               (msg/self r 'add)))))))
(define a-tree/self
  (node/self 15
             (node/self 15 (mt/self) (mt/self))
             (mt/self)))
;;; test the self
(msg/self a-tree/self 'add)

;;; extend the mt/self and node/self
(define (mt/self/size parent-maker)
  (let ((parent (parent-maker)))
    (lambda (m)
      (case m
        ((size) (lambda (self) 0))
        (else (parent m))))))
(define (node/self/size parent-maker v l r)
  (let ((parent (parent-maker v l r)))
    (lambda (m)
      (case m
        ((size) (lambda (self) (+ 1
                                  (msg/self l 'size)
                                  (msg/self r 'size))))
        (else (parent m))))))
(define a-tree/self/size
  (node/self/size node/self 
                  10 
                  (mt/self/size mt/self)
                  (node/self/size node/self
                                  15
                                  (mt/self/size mt/self)
                                  (mt/self/size mt/self))))
(msg/self a-tree/self/size 'size)
(msg/self a-tree/self/size 'add)

        
;;; TODO Prototype             