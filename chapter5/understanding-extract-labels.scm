;; note: another way to map label to corresponding instructions
;; (if (symbol? next-inst)
;;     (receive
;;         (cons (make-label-entry next-inst insts) labels) '())
;;     (receive
;;         labels
;;         (cons (make-instruction next-inst) insts))
;;     )
(define (extract-labels text receive)
  (define (make-label-entry label-name insts)
    (cons label-name insts))
  (define (make-instruction text)
    (cons text '()))
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (labels insts)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive
                   (cons (make-label-entry next-inst insts) labels)
                   insts)
               (receive
                   labels
                   (cons (make-instruction next-inst) insts))
               ))))))

(load "../testframe.scm")
(let ((text
       '(label1
         (inst1 arg)
         (inst2 arg)
         label2
         (inst3 arg)
         (inst4 arg)
         label3)))
  (let ((labels (extract-labels text (lambda (labels insts) labels)))
        (insts (extract-labels text (lambda (labels insts) insts))))
    (begin
      (assertequal?
       labels
       '((label1 ((inst1 arg)) ((inst2 arg)) ((inst3 arg)) ((inst4 arg)))
         (label2 ((inst3 arg)) ((inst4 arg)))
         (label3)))
      (assertequal?
       insts
       '(((inst1 arg))
         ((inst2 arg))
         ((inst3 arg))
         ((inst4 arg)))))))

