(load "interpreter.ss")

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
           (printf "~s works!\n" title)
           (errorf
            'test
            "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
            title 'tested-expression expected produced))))))

(define run-tests
  (lambda ()
    (test "if-value-of"
      (value-of
       '((lambda (x) (if (zero? x)
                         12
                         47))
         0)
       (lambda (y) (errorf 'value-of "unbound variable ~s" y)))
      12)))
