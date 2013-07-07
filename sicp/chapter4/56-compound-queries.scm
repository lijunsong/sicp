; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;

(and (supervisor ?name (Ben Bitdiddle))
     (address ?name ?where))

; b. all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary;
(and (salary ?person ?salary)
     (salary (Ben Bitdiddle) ?ben-salary)
     (lisp-value > ?ben-salary ?salary))

; c. all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job.
(and (supervisor ?name ?supervisor)
     (not (job ?supervisor (computer . ?x)))
     (job ?supervisor ?job))


