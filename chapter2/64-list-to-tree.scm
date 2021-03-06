(load "set-as-binary-tree.scm")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (display "partial-tree: ") (display elts) (display " ") (display n) (newline)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (display "left-size:") (display left-size) (newline)
        (display "--begin to get left-result--") (newline)
        (let ((left-result (partial-tree elts left-size)))
          (display "!get partial-tree") (display elts) (display " ") (display left-size) (newline)
          (display "left-result is: ") (display left-result) (newline)
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (display "left-tree: ") (display left-tree)
            (newline)
            (display "non-left-elts: ") (display non-left-elts)
            (newline)
            (display "right-size: ") (display right-size)
            (newline)
            (display "--begin to get right-result--") (newline)
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (display "this-entry: ") (display this-entry)
              (newline)
              (display "right-result") (display right-result)
              (newline)
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (display "right-tree:") (display right-tree) (newline)
                (display "remaining-elts: ") (display remaining-elts) (newline)
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

