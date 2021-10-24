#lang racket
(require simple-polynomial/base)

(displayln "Лаб8, завдання 5.1, Вакулко Ярослав, ІПЗ-41/1")

(define coeffs-P (list 1 0 -9)) ; коефіцієнти для першого многочлену
(define coeffs-R (list 1 0 0 -27)) ; коефіцієнти для другого многочлену

(define P (polynomial coeffs-P))
(displayln (~a "P(x) = " P))

(define R (polynomial coeffs-R))
(displayln (~a "R(x) = " R))

(displayln (~a "f(x) = " (poly-gcd P R))) 