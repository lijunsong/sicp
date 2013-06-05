(load "84-apply-raise.scm")

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))


  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ; 1.
  ;; representation for dense polynomials
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term))
           term-list)
          ((= (order term) (length term-list))
           (cons (coeff term) term-list))
          (else
           (adjoin-term term (cons 0 term-list)))))

  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (list (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ; end 1.

  ;; zero-polynomial?
  (define (zero-polynomial? p)
    (define (zero-terms? ts)
      (cond ((empty-termlist? ts) true)
            ((=zero? (coeff (first-term ts)))
             (zero-terms? (rest-terms ts)))
            (else false)))
    (zero-terms? (term-list p)))
  ;; ends
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (zero-polynomial? x)))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)


;;; tests begin
(load "../testframe.scm")

(asserttrue (=zero? (make-polynomial 'x '(0 0 0 0 0))))

(asserteq? false (=zero? (make-polynomial 'x '(1 0 0 0 0 0))))

; x^3 + (x^3 + x^1 + 1)
(assertequal? (add (make-polynomial 'x '(1 0 0 1))
                   (make-polynomial 'x '(1 0 1 1)))
              (make-polynomial 'x '(2 0 1 2)))

; [(y^3+4)x^2] + [(2y^3 + 5)x^2 ]
; = [(3y^3+9)x^2
(assertequal? (add (make-polynomial
                    'x
                    (list (make-polynomial 'y '(1 0 0 4))
                          0
                          0))
                   (make-polynomial
                    'x
                    (list (make-polynomial 'y '(2 0 0 5)) 0 0)))
              (make-polynomial
               'x
               (list (make-polynomial 'y '(3 0 0 9))
                     0
                     0)))


; raise Error here: (y + 3) + 0 => raise error

; [(y^3+4)x^2 + (y+3)x] + [(2y^3 + 5)x^2]
; = [(3y^3+9)x^2 + (y+3)x
;; (assertequal? (add (make-polynomial
;;                     'x
;;                     (list (make-polynomial 'y '(1 0 0 4))
;;                           (make-polynomial 'y '(1 3))
;;                           0))
;;                    (make-polynomial
;;                     'x
;;                     (list (make-polynomial 'y '(2 0 0 5)) 0 0)))
;;               (make-polynomial
;;                'x
;;                (list (make-polynomial 'y '(3 0 0 9))
;;                      (make-polynomial 'y '(1 3))
;;                      0)))

; (x^5 + 1) x^3 = (x^8 + x^3)
(assertequal? (mul (make-polynomial 'x '(1 0 0 0 0 1))
                   (make-polynomial 'x '(1 0 0 0)))
              (make-polynomial 'x '(1 0 0 0 0 1 0 0 0)))
