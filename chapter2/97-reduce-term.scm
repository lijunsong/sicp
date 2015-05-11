(load "88-poly-sub-extend.scm")

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


  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
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

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result  (div-terms (term-list p1)
                                  (term-list p2))))
          (list
           (make-poly (variable p1) (car result))
           (make-poly (variable p1) (cadr result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((g (gcd-terms (term-list p1)
                            (term-list p2))))
          (make-poly (variable p1) g))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))

  ; 1.
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((g (reduce-terms (term-list p1)
                               (term-list p2))))
          (list
           (make-poly (variable p1) (car g))
           (make-poly (variable p1) (cadr g))))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))
  ; end 1.
  
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

  ;; div-terms
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms
                        (sub-terms L1
                                   (mul-terms (list (make-term new-o new-c))
                                              L2))
                        L2)))
                  (let ((q (car rest-of-result))
                        (r (cadr rest-of-result)))
                     (list (cons (make-term new-o new-c) q)
                           r))
                  
                  ))))))

  (define (pseudoremainder-terms L1 L2)
    (define (integerize factor L)
      (map
       (lambda (x)
         (make-term (order x) (mul factor (coeff x)))) L))
    (let* ((factor (expt (coeff (first-term L2))
                         (+ 1
                            (order (first-term L1))
                            (- 0 (order (first-term L2))))))
           (result (div-terms (integerize factor L1) L2)))
      (cadr result)))

  (define (gcd-terms a b)
    (define (simplify terms)
      (let ((gcd-fator (apply gcd (map (lambda (x) (coeff x)) terms))))
        (map (lambda (term)
               (make-term (order term) (div (coeff term) gcd-fator)))
             terms)))
    (if (empty-termlist? b)
        (simplify a)
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (remainder-terms L1 L2)
    (let ((result (div-terms L1 L2)))
      (cadr result)))

  ; 2.
  
  (define (reduce-terms n d)
    (define (integerize factor L)
      (map
       (lambda (x)
         (make-term (order x) (mul factor (coeff x)))) L))
    (define (gcd-factor terms)
      (apply gcd (map (lambda (x) (coeff x)) terms)))
    (define (div-factor terms factor)
      (map (lambda (term)
             (make-term (order term) (div (coeff term) factor)))
           terms))
    
    (let* ((g (gcd-terms n d))
           (c (coeff (first-term g)))
           (o2 (order (first-term g)))
           (o1 (max (order (first-term n))
                    (order (first-term d))))
           (factor (expt c (+ 1 o1 (- 0 o2))))
           (integerizing-n (integerize factor n))
           (integerizing-d (integerize factor d)))
      (let* ((result-n (car (div-terms integerizing-n g)))
             ; NOTE: use car to fetch the divisor
             (result-d (car (div-terms integerizing-d g)))
             (coeff-gcd (gcd-factor (append result-n result-d))))
        (list (div-factor result-n coeff-gcd)
              (div-factor result-d coeff-gcd)))))

  ; end 2.
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


  (define (neg-term L)
    (make-term (order L) (neg (coeff L))))
  (define (neg-terms L)
    (if (null? L)
        '()
        (cons (neg-term (first-term L))
              (neg-terms (rest-terms L)))))

  
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

  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))))

  ; 3.
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (reduce-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  ; end 3.
  
  'done)


(put 'greatest-common-divisor '(scheme-number scheme-number)
     (lambda (a b)
       (gcd a b)))

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

; 4.
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(scheme-number scheme-number)
     (lambda (n d)
       (reduce-integers n d)))

(define (reduce n d)
  (apply-generic 'reduce n d))

; end 4.

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  ; 5.
  (define (make-rat n d)
    (reduce n d))
  ; end 5.
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (raise x)
    (make-complex-from-real-imag (div (numer x)
                                      (denom x))
                                 0))
  (put 'raise 'rational raise)

  (put '=zero? '(rational)
       (lambda (x) (equ? (numer x) 0)))  
  
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

(install-polynomial-package)
(install-rational-package)

;;; tests begin
(load "../testframe.scm")

(let ((p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
      (p2 (make-polynomial 'x '((2 11) (0 7))))
      (p3 (make-polynomial 'x '((1 13) (0 5)))))
  (let ((q1 (mul p1 p2))
        (q2 (mul p1 p3)))
    (assertequal? (reduce q1 q2)
                  (list p2 p3))))

(let ((p1 (make-polynomial 'x '((1 1)(0 1))))
      (p2 (make-polynomial 'x '((3 1)(0 -1))))
      (p3 (make-polynomial 'x '((1 1))))
      (p4 (make-polynomial 'x '((2 1)(0 -1)))))
  (let ((rf1 (make-rational p1 p2))
        (rf2 (make-rational p3 p4)))
    (asserttrue (or (equal? (add rf1 rf2)
                            (make-rational
                             (make-polynomial 'x '((3 1) (2 2) (1 3) (0 1)))
                             (make-polynomial 'x '((4 1) (3 1) (1 -1) (0 -1)))))
                    (equal? (add rf1 rf2)
                            (make-rational
                             (make-polynomial 'x '((3 -1) (2 -2) (1 -3) (0 -1)))
                             (make-polynomial 'x '((4 -1) (3 -1) (1 1) (0 1)))))))))

; previous: (rational (polynomial x (4 1) (3 1) (2 1) (1 -2) (0 -1)) (polynomial x (5 1) (3 -1) (2 -1) (0 1)))

; now: (rational (polynomial x (3 -1) (2 -2) (1 -3) (0 -1)) (polynomial x (4 -1) (3 -1) (1 1) (0 1)))

