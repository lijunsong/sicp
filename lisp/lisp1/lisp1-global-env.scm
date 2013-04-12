(load "../utils.scm")

(define env.init '())
(define env.global env.init)

(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin
       (display 'name)
       (set! env.global (cons (cons 'name 'void) env.global))
       'name))
    ((definitial name value)
     (begin
       (set! env.global (cons (cons 'name value) env.global))
       'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda (values)
         (if (= arity (length values))
             (begin (print "apply:" value "\n" values "\n")
                    (apply value values))
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


