;; constructor
(define (make-queue)
  (let ((front-ptr '())
        (rear-prt '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (car front-ptr))
    (define (insert-queue! item)
      (cond ((empty-queue?)
             (set! front-ptr (list item))
             (set! rear-ptr front-ptr)
             (car front-ptr))
            (else
             (set-cdr! rear-ptr (list item))
             (set! rear-ptr (cdr rear-ptr))
             (car front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error 'make-queue "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error 'make-queue "Unknown command -- MAKE-QUEUE"))))
    dispatch))

;; selector
(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (front-queue queue)
  ((queue 'front-queue)))

;; mutator
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(load "queue-regression-test.scm")

