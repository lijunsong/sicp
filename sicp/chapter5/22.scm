(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")

(define controller
  '((assign continue (label append-done))
    append-loop
      (test (op null?) (reg x))
      (branch (label base-case))

      (save continue)
      (assign continue (label after-append-cdr))
    
      (save x)
      (assign x (op cdr) (reg x))
      (goto (label append-loop))
      
    after-append-cdr
      (restore x)
      (assign x (op car) (reg x))
      (assign val (op cons) (reg x) (reg val))
      (restore continue)
      (goto (reg continue))
      
    base-case
      (assign val (reg y))
      (goto (reg continue))
      
    append-done
    ))

(define machine-append
  (make-machine
   '(x continue val y)
   (list (list 'null? null?)
         (list 'cdr cdr)
         (list 'car car)
         (list 'cons cons))
   controller))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(load "../testframe.scm")
(let ((x (list 'a 'b))
      (y (list 'c 'd)))
  (set-register-contents! machine-append 'x x)
  (set-register-contents! machine-append 'y y)
  (start machine-append)
  (assertequal? (get-register-contents machine-append 'val)
                (append x y)))

;; append! machine
(define machine-append!-wrong
  (make-machine
   '(x y tmp val)
   (list (list 'cdr cdr)
         (list 'set-cdr! set-cdr!)
         (list 'null? null?))
   '(last-pair
       (assign tmp (op cdr) (reg x)) 
       (test (op null?) (reg tmp))
       (branch (label last-pair-done))
       (assign x (op cdr) (reg x))
       (goto (label last-pair))
     last-pair-done
       (perform (op set-cdr!) (reg x) (reg y))
       (assign val (reg x))
     )))

(define machine-append!
  (make-machine
   '(x y x1 tmp val)
   (list (list 'cdr cdr)
         (list 'set-cdr! set-cdr!)
         (list 'null? null?))
   '((assign x1 (reg x))
     last-pair ;input x1, output val
       (assign tmp (op cdr) (reg x1)) 
       (test (op null?) (reg tmp))
       (branch (label last-pair-done))
       (assign x1 (op cdr) (reg x1))
       (goto (label last-pair))
     last-pair-done
       (perform (op set-cdr!) (reg x1) (reg y))
       (assign val (reg x))
     )))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(let* ((x (list 'a 'b))
       (y (list 'c 'd))
       (w (append x y)))
  (set-register-contents! machine-append! 'x x)
  (set-register-contents! machine-append! 'y y)
  (start machine-append!)
  ;; the content of x should have been changed now!
  (assertequal? (get-register-contents machine-append! 'x) w))
