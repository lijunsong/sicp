(load "../utils.scm")

(define env.init '())
(define env.global env.init)

(define fenv.init '())
(define fenv.global fenv.init)

(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin
       (set! env.global (cons (cons 'name 'void) env.global))
       'name))
    ((definitial name value)
     (begin
       (set! env.global (cons (cons 'name value) env.global))
       'name))))

(define-syntax definitial-function
  (syntax-rules ()
    ((definitial-function name)
     (begin
       (set! fenv.global (cons (cons 'name 'void) fenv.global))
       'name))
    ((definitial-function name value)
     (begin
       (set! fenv.global (cons (cons 'name value) fenv.global))
       'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial-function name
       (lambda (values)
         (if (= arity (length values)) 
             (apply value values)
             (error 'defprimitive "Incorrect arity"
                    (list 'name values))))))))

(definitial t #t)
(define the-false-value (cons "false" "boolean"))
(definitial f the-false-value)
(definitial nil '())
(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)


