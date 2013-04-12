
(define (make-mobile left right)
  (list left right))

;; selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))


(define (make-branch lengthh structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-strucuture branch)
  (cadr branch))

;;; total-weight: return the total weight of a mobile
(define (total-weight mobile)
  (define (branch-weight branch)
    (cond ((pair? (branch-strucuture branch)) ; it is a mobile
           (total-weight (branch-strucuture branch)))
          (else
           (branch-strucuture branch))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define test-mobile
  (make-mobile
   (make-branch 2 (make-mobile (make-branch 4 3)
                               (make-branch 6 12)))
   (make-branch 3 (make-mobile (make-branch 19 20)
                               (make-branch 22 31)))))
; assert (total-weight test-mobile) == 66


;;; balanced? test case:
(define balanced-mobile
  (make-mobile (make-branch 100 (make-mobile (make-branch 4 10)
                                             (make-branch 5 10)))
               (make-branch 20 (make-mobile (make-branch 6 50)
                                            (make-branch 7 50)))))

;;; got an error here.
(define (balanced? mobile)
  (define (branch-weight branch)
    (if (pair? (branch-strucuture branch))
        (total-weight (branch-strucuture branch))
        (branch-strucuture branch)))
  (let ((left-weight (branch-weight (left-branch mobile)))
        (left-len (branch-length (left-branch mobile)))
        (right-weight (branch-weight (right-branch mobile)))
        (right-len (branch-length (right-branch mobile))))
    (if (= (* left-weight left-len)
           (* right-weight right-len))
        #t
        #f)))
