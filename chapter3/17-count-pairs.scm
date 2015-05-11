(define (count-pairs x)
  (define (pair-in-list? x lst)
    (cond ((null? lst) #f)
          ((eq? x (car lst)) #t) ;; compare its pointer
          (else (pair-in-list? x (cdr lst)))))
  
  (define (record x visited)
    (cond ((not (pair? x)) visited)
          ((pair-in-list? x visited)
           (let* ((car-visited (record (car x) visited))
                  (cdr-visited (record (cdr x) car-visited)))
             cdr-visited))
          (else
           (let* ((include-x (cons x visited))
                  (car-visited (record (car x) include-x))
                  (cdr-visited (record (cdr x) car-visited)))
             cdr-visited))))
  (length (record x '())))

(define count3 (list 1 2 3))

(define count4
  (let ((pairs (list 1 2 3)))
    (begin
      (set-car! pairs (cddr pairs))
      pairs)))

(define count5
  (let ((pairs (list 1 2 3)))
    (begin
      (set-car! pairs (cddr pairs))
      (set-car! (cdr pairs) (cddr pairs))
      pairs)))

(define count7
  (let ((pairs (list 1 2 3)))
    (begin
      (set-car! pairs (cdr pairs))
      (set-car! (cdr pairs) (cddr pairs))
      pairs)))


;;; tests begin
(load "../testframe.scm")

(assert= (count-pairs count3) 3)
(assert= (count-pairs count4) 3)
(assert= (count-pairs count5) 3)
(assert= (count-pairs count7) 3)
