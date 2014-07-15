(data-path
 (register
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))
 (operation
  ((name rem)
   (input (register a) (register b)))
  ((name =)
   (input (register b) (constant 0)))))

(controller
 test-b
   (test =)
   (branch (label gcd-done))
   (t<-r)
   (a<-b)
   (b<-t)
   (goto test-b)
 gcd-done)

;; a better way. I just realize this is just the assembly language.

(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)
