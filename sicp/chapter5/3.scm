; 1.

(controller
    (assign (reg g) (const 1.0))
 test-g
    (test (op good-enough?) (reg g))
    (branch (label done))
    (assign (reg t) (op improve) (reg g))
    (assign (reg g) (reg t))
    (goto (label test-g))
 done)

; 2.

(controller
    (assign (reg g) (const 1.0))
 test-g
    ; good-enough?
    (assign (reg t1) (op square) (reg g))
    (assign (reg t2) (op -) (reg t1) (reg x))
    (assign (reg t3) (op abs) (reg t2))
    (test (op <) (reg t3) (const 0.01))
    (branch (label done))
    
    (assign (reg t) (op improve) (reg g))
    (assign (reg g) (reg t))
    (goto (label test-g))
 done)

; 3.
(controller
    (assign (reg g) (const 1.0))
 test-g
    ; good-enough?
    (assign (reg t1) (op square) (reg g))
    (assign (reg t2) (op -) (reg t1) (reg x))
    (assign (reg t3) (op abs) (reg t2))
    (test (op <) (reg t3) (const 0.01))
    (branch (label done))
    ; improve
    (assign (reg t4) (op /) (reg x) (reg g))
    (assign (reg t) (op average) (reg g) (reg t4))
    
    (assign (reg g) (reg t))
    (goto (label test-g))
 done)

