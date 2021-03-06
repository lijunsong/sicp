(load "get-put.scm")

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (contents datum)
  (cond ((pair? datum)
         (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)
         'scheme-number)
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

; raise
(define (raise x)
  (let ((f (get 'raise (type-tag x))))
    (if f
        (f (contents x))
        (error "RAISE ERROR" x))))

(define (successive-raise type-datum top-type)
  (if (eq? (type-tag type-datum) top-type)
      type-datum
      (successive-raise (raise type-datum) top-type)))

(define tower '(scheme-number rational complex))

; highest type
(define (the-most-top-type type-tags)
  (define (uppers tags upper-tower)
    (cond ((null? tags) (car upper-tower))
          (else
           (let ((maybe-uppers (member (car tags) upper-tower)))
             (if maybe-uppers
                 (uppers (cdr tags) maybe-uppers)
                 (uppers (cdr tags) upper-tower))))))
  (uppers type-tags tower))

; new apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (>= (length args) 2)
              (let* ((type-tags (map type-tag args))
                     (top-type (the-most-top-type type-tags))
                     (coerced-args (map (lambda (x)
                                          (successive-raise x top-type))
                                        args))
                     (new-type-tags (map type-tag coerced-args)))
                (let ((proc (get op new-type-tags)))
                  (if proc
                      (apply proc (map contents coerced-args))
                      (error "No method for these types"
                             (list op type-tags)))))
              (error "No method for these types -- APPLY-GENERIC"
                     (list op type-tags)))))))
; end

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (=zero? (sub x y)))
(define (neg x) (apply-generic 'neg x))

;;;;;;;;;;;;;;; packages

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  ; 1. raise number to rational
  (define (raise x)
    (make-rational x 1))
  (put 'raise 'scheme-number raise)
  ; end 1.
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))

  (put 'neg '(scheme-number)
       (lambda (x) (- 0 x)))
  
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (raise x)
    (make-complex-from-real-imag (/ (numer x)
                                    (denom x))
                                 0))
  (put 'raise 'rational raise)

  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))  

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'neg '(rational)
       (lambda (x) (tag (make-rat (neg (numer x))
                                  (denom x)))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '=zero? '(rectangular)
       (lambda (x) (= 0 (real-part x))))
  (put 'neg '(rectangular)
       (lambda (x) (tag (make-from-real-imag
                         (neg (real-part x))
                         (neg (imag-part x))))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '=zero? '(polar)
       (lambda (x) (= 0 (real-part x))))

  (put 'neg '(polar)
       (lambda (x) (tag (make-from-mag-ang
                         (neg (magnitude x))
                         (angle x)))))
  
  'done)
(install-polar-package)
(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '=zero? '(complex)
       (lambda (x) (=zero? x)))
  (put 'neg '(complex)
       (lambda (x) (tag (neg x))))
  
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;;;;
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)


;;; regression tests 

(load "../testframe.scm")

(assert= (add 3 3) 6)
(assertequal? (add 3 (make-rational 1 4))
              (make-rational 13 4))
(assertequal? (add 3 (make-complex-from-real-imag 3 5))
              (make-complex-from-real-imag 6 5))

(assertequal? (add (make-rational 2 5)
                   (make-rational 1 5))
              (make-rational 3 5))

(assertequal? (add (make-complex-from-real-imag 3 8)
                   (make-complex-from-real-imag 5 6))
              (make-complex-from-real-imag 8 14))

;;; tests neg
(assert= (neg 3) -3)
(asserttrue (equ? (neg (make-rational 1 4)) (sub 0 (make-rational 1 4))))
(asserttrue (equ? (neg (make-complex-from-real-imag 1 3))
                  (make-complex-from-real-imag -1 -3)))

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

  ; 1. 
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ; end 1.
  
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

  ; 2. 
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (neg-terms L2))
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (neg-term t2) (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  ; end 2.
  
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
  
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ; 3.
  (define (neg-term L)
    (make-term (order L) (neg (coeff L))))
  (define (neg-terms L)
    (if (null? L)
        '()
        (cons (neg-term (first-term L))
              (neg-terms (rest-terms L)))))
  ; end 3.
  
  (define (zero-polynomial? p)
    (define (zero-terms? ts)
      (cond ((empty-termlist? ts) true)
            ((=zero? (coeff (first-term ts)))
             (zero-terms? (rest-terms ts)))
            (else false)))
    (zero-terms? (term-list p)))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (zero-polynomial? x)))

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)


;;; regression tests
(asserttrue (=zero? (make-polynomial 'x '((100 0) (2 0) (0 0)))))

(asserteq? false (=zero? (make-polynomial 'x '((100 0) (2 0) (0 1)))))

(assertequal? (add (make-polynomial 'x '((100 1) (2 2) (0 1)))
                   (make-polynomial 'x '((100 1) (2 2) (0 1))))
              (make-polynomial 'x '((100 2) (2 4) (0 2))))

; mul requires the raise (or drop) implemented in 2.84 or 2.85.

;; (mul (make-polynomial 'x
;;                       (list (list 2 3)
;;                             (list 1 (make-complex-from-real-imag 2 3))
;;                             (list 0 7)))
;;      (make-polynomial 'x
;;                       (list (list 4 1)
;;                             (list 2 (make-rational 2 3))
;;                             (list 0 (make-complex-from-real-imag 5 3)))))

;;; tests begin

(assertequal? (sub (make-polynomial 'x '())
                   (make-polynomial 'x '((2 1) (1 10) (0 12))))
              (make-polynomial 'x '((2 -1) (1 -10) (0 -12))))

(assertequal? (sub (make-polynomial 'x '((2 2)))
                   (make-polynomial 'x '((2 1) (1 10) (0 12))))
              (make-polynomial 'x '((2 1) (1 -10) (0 -12))))
