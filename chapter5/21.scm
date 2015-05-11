(load "machine-stack-module.scm")
(load "machine-register-module.scm")
(load "machine-machine-module.scm")
(load "machine-assemble-module.scm")


;; a
(define controller-a
  '((assign continue (label count-done))
    count-loop
      (test (op null?) (reg tree))
      (branch (label base-case0))

      (assign t (op pair?) (reg tree))
      (test (op not) (reg t))
      (branch (label base-case1))

      (save continue)
      (assign continue (label count-after-car))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label count-loop))

    count-after-car
      (restore tree)
      (assign tree (op cdr) (reg tree)) ; prepare for (count-leaves (cdr tree))
      (save val)                        ; value of (count-leaves (car tree))

      (assign continue (label count-after-cdr))
      (goto (label count-loop))

    count-after-cdr
      (assign tree (reg val))         ; val stores value of (count-leaves (cdr tree))
      (restore val)                   ; get value of (count-leaves (car tree))
      (restore continue)
      (assign val (op +) (reg tree) (reg val))  ; put the result in val
      (goto (reg continue))

    base-case0
      (assign val (const 0))
      (goto (reg continue))

    base-case1
      (assign val (const 1))
      (goto (reg continue))

    count-done
))
;; end controller-a

(define machine-a
  (make-machine
   '(tree t continue val)
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'not not)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +))
   controller-a))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; tests for machine-a begin
(load "../testframe.scm")
(let ((tree '((1 (2 3 4)) ((5) (6) (((7)))))))
  (set-register-contents! machine-a 'tree tree)
  (start machine-a)
  (assert= (get-register-contents machine-a 'val)
           (count-leaves tree)))

;; b
(define controller-b
  '((assign n (const 0))
    (assign continue (label count-done))
    count-loop
      (test (op null?) (reg tree))
      (branch (label base-case0))
      
      (assign t (op pair?) (reg tree))
      (test (op not) (reg t))
      (branch (label base-case1))

      (save continue)
      (assign continue (label count-after-car))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label count-loop))
      
    count-after-car
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (assign continue (label count-after-cdr))
      (goto (label count-loop))
      
    count-after-cdr
      (restore continue)
      
    base-case0
      ;; n value is kept in reg n
      (goto (reg continue))
    base-case1
      (assign n (op +) (reg n) (const 1))
      (goto (reg continue))
    count-done
    ))
;; end controller-b

(define machine-b
  (make-machine
   '(tree t continue n)
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'not not)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +))
   controller-b))

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

;; tests for machine-b begin
(load "../testframe.scm")
(let ((tree '((1 (2 3 4)) ((5) (6) (((7)))))))
  (set-register-contents! machine-b 'tree tree)
  (start machine-b)
  (assert= (get-register-contents machine-b 'n)
           (count-leaves tree)))
