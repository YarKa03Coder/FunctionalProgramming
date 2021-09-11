#| Task 5.1 |#

(define recursion-depth 0)

(define (euclidian-algorithm m n depth)
  (if (> n m)
      (euclidian-algorithm m (modulo n m) (+ depth 1))
      (if (= n 0)
          (begin (set! recursion-depth (+ depth 1)) m)
          (euclidian-algorithm n (modulo m n) (+ depth 1)))))

(euclidian-algorithm 7 50 0)
(display "Recursion depth = ")
(display recursion-depth)
(newline)

#| Task 5.2 |#
(define (display-numbers x)
  (if (or (= x 0) (< x 0))
      1
      (begin (display (modulo x 10)) (display " ") (display-numbers(quotient x 10)))))

(display-numbers (read))