(list 1 (list 2 (list 3 4)))

; (1 (2 (3 4)))


; grab 7
(cadr (caddr '(1 3 (5 7) 9)))

(car (car '((7))))

(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))
