(define (make-mobile left right)
  (list left right))

;;; a)
;; selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-strucuture branch)
  (cadr branch))

;; a) tests begin
(load "../testframe.scm")

(let ((mobile (make-mobile (make-branch 2 34)
                           (make-branch 3 45))))
  (assert= (branch-length (left-branch mobile)) 2)
  (assert= (branch-strucuture (right-branch mobile)) 45))

;; a) tests end

;;; b)
;; total-weight: return the total weight of a mobile
(define (total-weight mobile)
  (define (branch-weight branch)
    (cond ((pair? (branch-strucuture branch)) ; it is a mobile
           (total-weight (branch-strucuture branch)))
          (else
           (branch-strucuture branch))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; a finer one
(define (total-weight mobile)
  (define (branch-weight branch)
    (cond ((number? (branch-strucuture branch))
           (branch-strucuture branch))
          (else
           (total-weight (branch-strucuture branch)))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; b) tests begin
(let ((test-mobile
       (make-mobile
        (make-branch 2 (make-mobile (make-branch 4 3)
                                    (make-branch 6 12)))
        (make-branch 3 (make-mobile (make-branch 19 20)
                                    (make-branch 22 31))))))
  (assert= (total-weight test-mobile) 66))
;; b) tests end

;;; c)
;; balanced? test case:

; submobile is not balanced!
(define unbalanced-mobile
  (make-mobile (make-branch 100 (make-mobile (make-branch 4 10)
                                             (make-branch 5 10)))
               (make-branch 20 (make-mobile (make-branch 6 50)
                                            (make-branch 7 50)))))
(define balanced-mobile-1
  (make-mobile (make-branch 100 (make-mobile (make-branch 100 20)
                                             (make-branch 20 100)))
               (make-branch 20 (make-mobile (make-branch 10 300)
                                            (make-branch 10 300)))))

(define balanced-mobile-2
  (make-mobile (make-branch 100 22)
               (make-branch 20 (make-mobile (make-branch 10 100)
                                            (make-branch 100 10)))))

(define balanced-mobile-3
  (make-mobile (make-branch 100 22)
               (make-branch 22 100)))

(define (balanced? mobile)
  (define (branch-weight branch)
    (if (number? (branch-strucuture branch))
        (branch-strucuture branch)
        (total-weight (branch-strucuture branch))))
  (cond ((number? mobile) #t)
        (else
         (let ((left (left-branch mobile))
               (right (right-branch mobile)))
           (cond ((not (= (* (branch-length left)
                             (branch-weight left))
                          (* (branch-length right)
                             (branch-weight right))))
                  #f)
                 ((not (and (balanced? (branch-strucuture left))
                            (balanced? (branch-strucuture right))))
                  #f)
                 (else #t))))))


;; c) tests begin
(asserteq? (balanced? unbalanced-mobile) #f)
(asserteq? (balanced? balanced-mobile-1) #t)
(asserteq? (balanced? balanced-mobile-2) #t)
(asserteq? (balanced? balanced-mobile-3) #t)


