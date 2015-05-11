(rule (same ?x ?x))

(rule (replace ?person1 ?person2)
      (and (not (same ?person1 ?person2))
           (job ?person1 ?person1-job)
           (job ?person2 ?person2-job)
           (or (same ?person1-job ?person2-job)
               (and (job ?another-person ?person1-job)
                    (job ?another-person ?person2-job)))))

; a.  all people who can replace Cy D. Fect;
(replace ?person (Cy D. Fect))

; b.  all people who can replace someone who is being paid more than they are, together with the two salaries.
(and (salary ?person1 ?salary1)
     (salary ?person2 ?salary2)
     (and (replace ?person1 ?person2)
          (lisp-value > ?salary2 ?salary1)))
