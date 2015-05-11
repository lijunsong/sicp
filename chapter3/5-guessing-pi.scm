(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (in-circle?)
    (p (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (let ((retangle-area (* (abs (- x2 x1))
                          (abs (- y2 y1))))
        (rate (monte-carlo trials in-circle?))
        (radius (/ (abs (- x2 x1)) 2.0)))
    (/ (* retangle-area rate)
       (* radius radius))))


(let ((x1 2)
      (y1 4)
      (x2 8)
      (y2 10)
      (p (lambda (x y)
           (<= (+ (* (- x 5)
                     (- x 5))
                  (* (- y 7)
                     (- y 7)))
               (* 3 3))))
      (trials 100000))
  (estimate-integral p x1 x2 y1 y2 trials))




