(load "get-put.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         (let ((op (get 'deriv (operator exp))))
           (if op
               (op (operands exp) var)
               (else (error "NOT IMPLEMENTED OPERATOR -- " exp)))))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator e)
  (car e))

(define (operands e)
  (cdr e))

(define (tag-operator t . e)
  (cons t e))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define sum-symbol '+)
(define product-symbol '*)
(define exponent-symbol '**)

;;; packages developed separately.
;; sum
(define (install-sum-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (tag a1 a2))))

  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  ;;put into table
  (define (tag s1 s2) (tag-operator sum-symbol s1 s2))
  (put 'deriv sum-symbol deriv-sum)
  (put 'constructor sum-symbol make-sum))

;; product
(define (install-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (tag m1 m2))))
  
  (define (multiplier p) (car p))

  (define (multiplicand p)
    (cadr p))

  (define (deriv-product exp var)
    ((get 'constructor sum-symbol)
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  ;; rest of the package

  (define (tag m1 m2) (tag-operator product-symbol m1 m2))
  
  (put 'deriv product-symbol deriv-product)
  (put 'constructor product-symbol make-product))

;; install
(install-sum-package)
(install-product-package)
