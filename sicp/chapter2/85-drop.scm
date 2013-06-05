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

; same-type
(define (same-type d1 d2)
  (eq? (type-tag d1) (type-tag d2)))

; drop
(define (drop x)
  ((get 'drop (type-tag x)) (contents x)))

(define (successive-drop x)
  (let ((new (drop x)))
    (if (same-type new x)
        x
        (successive-drop new))))

; new apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (successive-drop
           (apply proc (map contents args)))
          (if (>= (length args) 2)
              (let* ((type-tags (map type-tag args))
                     (top-type (the-most-top-type type-tags))
                     (coerced-args (map (lambda (x)
                                          (successive-raise x top-type))
                                        args))
                     (new-type-tags (map type-tag coerced-args)))
                (let ((proc (get op new-type-tags)))
                  (if proc
                      (successive-drop
                       (apply proc (map contents coerced-args)))
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

;;;;;;;;;;;;;;; packages

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (define (raise x)
    (make-rational x 1))
  (put 'raise 'scheme-number raise)
; 1.
  (put 'drop 'scheme-number
       (lambda (x)
         (make-scheme-number (contents x)))) ; drop to itself
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

; 2.
  (define (drop x)
    (if (= 1 (denom x))
        (make-scheme-number (numer x))
        (make-rational (numer x)
                       (denom x))))
  (put 'drop 'rational drop)
; end 2.
  
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

; 3.
  (put 'drop 'rectangular
       (lambda (x)
         (if (= 0 (imag-part x))
             (make-scheme-number (real-part x))
             (make-complex-from-real-imag (real-part x)
                                          (imag-part x)))))
; end 3.
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
  ; 4.
  (put 'drop 'polar
       (lambda (x)
         (if (= 0 (imag-part x))
             (make-scheme-number (real-part x))
             (make-complex-from-mag-ang (magnitude x)
                                        (angle x)))))
  ; end 4.
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
  ; 5.
  (put 'drop 'complex 
       (lambda (x) (drop x)))
  ; end 5.
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;;;;
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)


;;; TODO: it has problems to be used in poly.scm

;;; tests begin

(load "../testframe.scm")

;; regression test
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


;; tests for drop
(assert= (add (make-rational 1 2)
              (make-rational 1 2)) 1)

(assert= (sub (make-complex-from-real-imag 3 4)
              (make-complex-from-real-imag 2 4))
         1)

(assert= (add 3 3) 6)





