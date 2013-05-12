(define (wrong . msg)
  (error 'wrong msg))

;;; check the equality of e1 and e2
(define-syntax assert-equal
  (syntax-rules ()
    ((assert-equal f e1 e2)
     (if (not (f e1 e2))
         (wrong 'f e1 e2)))))

(define (asserteq? e1 e2)
  (assert-equal eq? e1 e2))

(define (assert= e1 e2)
  (assert-equal = e1 e2))

(define (assertequal? e1 e2)
  (assert-equal equal? e1 e2))

(define (asserttrue e)
  (asserteq? #t e))

(define (assertfalse e)
  (asserteq? #f e))

;; TODO: what about set? 

;; f can be a function used to compare the ele to the elements in lst
(define (assertin ele lst . f)
  (let* ((func (cond ((not (null? f)) (car f))
                     ((symbol? ele) eq?)
                     (else equal?)))
         (in? (member-procedure func)))
    (if (not (in? ele lst))
        (wrong 'assertin ele lst))))

;;; test the equality of output, the argument can be string, or expression
(define-syntax assert-output-equal
  (syntax-rules ()
    ((assert-output-equal e1 e2)
     (let ((str1 (cond ((pair? 'e1)
                        (with-output-to-string
                          (lambda ()
                            e1)))
                       ((string? 'e1)
                        e1)
                       (else (wrong 'assert-output-equal "type error: " e1))))
           (str2 (cond ((pair? 'e2)
                        (with-output-to-string
                          (lambda ()
                            e2)))
                       ((string? 'e2)
                        e2)
                       (else (wrong 'assert-output-equal "type error: " e2)))))
       (assert-equal equal? str1 str2)))))

;;; procedures that handle simple-error
(define (irritants cdt)
  (if (not (condition? cdt))
      (error 'internal-error cdr " is not condition")
      ((condition-accessor condition-type:simple-error 'irritants) cdt)))

(define-syntax fetch-irritants
  (syntax-rules ()
    ((fetch-irritants expr)
     (irritants (ignore-errors (lambda () expr))))))

;;; main assert exception procedure
; (string or expr * string or expr) =>
; note: the string can be part of the string of irritants !
(define-syntax assert/exn
  (syntax-rules ()
    ((assert/exn e1 e2)
     (let ((maybe-irritant1
            (cond ((pair? 'e1) (fetch-irritants e1))
                  ((string? 'e1) e1)
                  (else (wrong 'assert/exn "type error: " 'e1))))
           (maybe-irritant2
            (cond ((pair? 'e2) (fetch-irritants e2))
                  ((string? 'e2) e2)
                  (else (wrong 'assert/exn "type error: " 'e2)))))
       (cond ((and (pair? maybe-irritant1)
                   (pair? maybe-irritant2)) ;which means both are expr
              (assertequal? maybe-irritant1 maybe-irritant2))
             ((pair? maybe-irritant1) ; which menas maybe-irritant2 is a str
              (assertin maybe-irritant2
                        maybe-irritant1
                        (lambda (x y)
                          (or (substring? x y) ;TODO: x or y may not be necessarily a string!
                              (substring? y x)))))
             ((pair? maybe-irritant2) ; which means maybe-irritant1 is a str
              (assertin maybe-irritant1 maybe-irritant2))
             (else ; both are string
              (assertequal? maybe-irritant1 maybe-irritant2)))))))

'testframe-loaded
