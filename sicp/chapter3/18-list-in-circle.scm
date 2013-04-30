(define (circle? lst)
  (define (pair-in-list? x lst)
    (cond ((null? lst) #f)
          ((eq? x (car lst)) #t) ;; compare its pointer
          (else (pair-in-list? x (cdr lst)))))

  (define (circle/rec? l visited)
    (cond ((null? l) #f)
          ((pair-in-list? l visited) #t)
          (else
           (circle/rec? (cdr l) (cons l visited)))))
  (circle/rec? lst '()))

;;; tests begin

(let ((pairs '(1 2 3)))
  (begin
    (set-cdr! (cddr pairs) (cdr pairs))
    (asserteq? (circle? pairs) #t)))

(let ((pairs '(1 2 3)))
  (begin
    (set-cdr! (cddr pairs) pairs)
    (asserteq? (circle? pairs) #t)))

(let ((pairs '(1 2 3)))
  (begin
    (set-cdr! (cddr pairs) (cddr pairs))
    (asserteq? (circle? pairs) #t)))


(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))  
  (set-cdr! (last-pair x) x)
  x)

(let ((z (make-cycle (list 'a 'b 'c))))
  (asserteq? (circle? z) #t))
