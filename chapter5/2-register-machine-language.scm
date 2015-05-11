; TODO: declare the register and operation!
(data-path
 (registers
  ((name counter)
   ;...
   ))
 (operations
  ((name +)
   ())
  ((name *)
   ())
  ((name >)
   ())))

(constroller
    (assign product (const 1))
    (assign counter (const 1))
 test-p
    (test (op >) (reg counter) (const 0))
    (branch (label done))
    (assign t2 (op +) (reg counter) (const 1))
    (assign t1 (op *) (reg product) (reg counter))
    (assign product (reg t1))
    (assign counter (reg t2))
    (goto (label test-p))
 done)
 
