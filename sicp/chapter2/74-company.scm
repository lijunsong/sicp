; structure: 
; (("name" ((item1 . "item1") (item2 . "item2") ...))
;  ("name2" ...)
;; get-record
(define (get-record name records)
  (cond ((null? records) #f)
        ((string=? (car (car records)) name)
         (car records))
        (else
         (get-record (cadr records) name))))

; structure:
; record = ((item1 . "item1") (his-salary . "item2")...)
;
; (get-salary 'his-salary record)
;; get-salary
(define (get-salary key record)
  (cond ((null? record) #f)
        ((eq? key (car (car record))) (cdr (car record)))
        (else
         (get-salary key (cdr record)))))

;; find-employee-record
(define (find-employee-record name record-files)
  (cond ((null? record-files) '())
        (else
         (let ((record (get-record name (car record-files))))
           (if record
               (cons record (find-employee-record name (cdr record-files)))
               (find-employee-record name (cdr record-files)))))))
