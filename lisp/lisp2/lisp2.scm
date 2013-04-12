(load "lisp2-global-env.scm")
(load "../utils.scm")

(define (lookup sym env)
  (begin
    (if (pair? env)
             (if (eq? sym (caar env))
                 (cdar env)
                 (lookup sym (cdr env)))
             (error 'lookup "No such binding" sym env))))

(define empty-begin 813)
(define (f.eprogn exps env fenv)
  (begin (print "f.eprogn: " exps)
         (cond ((null? exps) empty-begin)
               ;; important!
               ((null? (cdr exps))
                (f.evaluate (car exps) env fenv))
               (else
                (begin
                  (f.evaluate (car exps) env fenv)
                  (f.eprogn (cdr exps) env))))))

(define (update! id env val)
  (cond ((null? env)
         (error 'update! "No such binding: ~s" id))
        ((eq? id (caar env))
         (begin
           (set-cdr! (car env) val) 
           val))
        (else
         (update! id (cdr env) val))))

(define (f.evlis exps env fenv)
  (cond ((null? exps) '())
        (else
         (cons (f.evaluate (car exps) env fenv)
               (f.evlis (cdr exps) env fenv)))))

(define (extend env ids vals)
  (cond ((null? ids) env)
        (else
         (cons (cons (car ids) (car vals))
               (extend env (cdr ids) (cdr vals))))))

(define (f.make-function args body env fenv)
  (lambda (vals)
    (f.eprogn body (extend env args vals) fenv)))

(define (invoke f args)
  (begin (print "invoke: " f " with args: " args)
         (if (procedure? f)
             (f args)
             (error 'f.evaluate "Not a function ~s" f))))

;;; Common Lisp: only symbol or lambda can appear in the function 
(define (f.evaluate-application fn args env fenv)
  (begin )
  (cond ((symbol? fn)
         (invoke (lookup fn fenv) args))
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (f.eprogn (cddr fn)
                   (extend env (cadr fn) args)
                   fenv))
        (else (error 'f.evaluate-application
                     "f.evaluate-application error"
                     fn args))))

(define (f.evaluate e env fenv)
  (if (not (pair? e))
      (cond ((symbol? e) (lookup e env))
            ((or (number? e)
                 (string? e)
                 (char? e)
                 (boolean? e)  
                 (vector e))
             e)
            (else (error "Can't f.evaluate the expression" e)))
      (case (car e)
        ((quote) (cdr e))
        ((if) (if (not (eq? (f.evaluate (cadr e) env fenv) the-false-value))
                  (f.evaluate (caddr e) env fenv)
                  (f.evaluate (cadddr e) env fenv)))
        ((begin) (f.eprogn (cdr e) env fenv))
        ((set!)
         (update! (cadr e) env (f.evaluate (caddr e) env fenv)))
        ((lambda) (f.make-function (cadr e) (cddr e) env fenv))
        (else
         (f.evaluate-application
          (car e) (f.evlis (cdr e) env fenv) env fenv)))))

(define (chapter2-scheme)
  (define (toplevel)
    (let ((s (read)))
      (cond ((eq? s 'exit)
             (display "quit")
             (newline))
            (else 
             (display (f.evaluate s env.global fenv.global))
             (newline)
             (toplevel)))))
  (toplevel))
