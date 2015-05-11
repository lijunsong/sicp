(load "amb.scm")

(define-variable!
  'multiple-dwelling
  (make-procedure
   '()
   (analyze-sequence
    (lambda-body
     '(lambda ()
        (let ((baker (amb 1 2 3 4 5))
              (cooper (amb 1 2 3 4 5))
              (fletcher (amb 1 2 3 4 5))
              (miller (amb 1 2 3 4 5))
              (smith (amb 1 2 3 4 5)))
          (require (not (= cooper 1)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (require (not (= baker 5)))
          (require (not (= fletcher 5)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))        
        ))) the-global-environment)
  the-global-environment)

(define-variable!
  'member
  (make-procedure
   '(x lst)
   (analyze-sequence
    (lambda-body
     '(lambda (x lst)
        (cond ((null? lst) false)
              ((= x (car lst)) true)
              (else (member x (cdr lst))))))) the-global-environment)
  the-global-environment)

(define-variable!
  'distinct?
  (make-procedure
   '(items)
   (analyze-sequence
    (lambda-body
     '(lambda (items)
        (cond ((null? items) true)
              ((null? (cdr items)) true)
              ((member (car items) (cdr items)) false)
              (else (distinct? (cdr items))))
        ))) the-global-environment)

  the-global-environment)


(let ((start (runtime)))
  (let ((run 
         (ambeval '(multiple-dwelling)
                the-global-environment                
                (lambda (val fail)
                  (user-print val))
                (lambda () (announce-output ";;; Finish")))))
    (let ((end (runtime)))
      (dp "start:" start)
      (dp "end:  " end))))
