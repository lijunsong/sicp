(load "machine-assemble-module.scm")

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (labels insts)
         (let ((next-inst (car text)))
           (cond ((and (symbol? next-inst) (assoc next-inst labels))
                  (error 'extract-labels "Duplicate labels: " next-inst))
                 ((symbol? next-inst)
                  (receive
                      (cons (make-label-entry next-inst insts) labels)
                      insts))
                 (else 
                  (receive
                      labels
                      (cons (make-instruction next-inst) insts)))))))))
(let ((text
       '(start
         (goto (label here))
         here
         (assign a (const 3))
         (goto (label there))
         here
         (assign a (const 4))
         (goto (label there))
         there)))
  (assert/exn
   (extract-labels text (lambda (labels insts) labels))
   "Duplicate"))
