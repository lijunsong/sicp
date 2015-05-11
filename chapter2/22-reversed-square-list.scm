
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))


;;1 ]=> (trace iter)

;;Unspecified return value

;;1 ]=> (square-list '(1 2 3 4 5))

;; [Entering #[compound-procedure 3 iter]
;;     Args: (1 2 3 4 5)
;;           ()]
;; [Entering #[compound-procedure 3 iter]
;;     Args: (2 3 4 5)
;;           (1)]
;; [Entering #[compound-procedure 3 iter]
;;     Args: (3 4 5)
;;           (4 1)]
;; [Entering #[compound-procedure 3 iter]
;;     Args: (4 5)
;;           (9 4 1)]
;; [Entering #[compound-procedure 3 iter]
;;     Args: (5)
;;           (16 9 4 1)]
;; [Entering #[compound-procedure 3 iter]
;;     Args: ()
;;           (25 16 9 4 1)]
;; [(25 16 9 4 1)
;;       <== #[compound-procedure 3 iter]
;;     Args: ()
;;           (25 16 9 4 1)]
;; [(25 16 9 4 1)
;;       <== #[compound-procedure 3 iter]
;;     Args: (5)
;;           (16 9 4 1)]
;; [(25 16 9 4 1)
;;       <== #[compound-procedure 3 iter]
;;     Args: (4 5)
;;           (9 4 1)]
;; [(25 16 9 4 1)
;;       <== #[compound-procedure 3 iter]
;;     Args: (3 4 5)
;;           (4 1)]
;; [(25 16 9 4 1)
;;       <== #[compound-procedure 3 iter]
;;     Args: (2 3 4 5)
;;           (1)]
;; [(25 16 9 4 1)
;;       <== #[compound-procedure 3 iter]
;;     Args: (1 2 3 4 5)
;;           ()]
;;Value 4: (25 16 9 4 1)
;;NOTE: this is one method to reverse a list

;;; another way:
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;1 ]=> (square-list-2 '(1 2 3 4 5))

;Value 7: (((((() . 1) . 4) . 9) . 16) . 25)

;;; the right way:
(define (square-list-right items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer 
                      (list (square (car things)))))))
  (iter items '()))

;1 ]=> (square-list-right '(1 2 3 4 5))

;Value 8: (1 4 9 16 25)

