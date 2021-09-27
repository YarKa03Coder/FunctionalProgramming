#lang racket

;ІПЗ-41/1, Вакулко Ярослав
(display "Лаб3, завдання 5.1-5.2, Вакулко Ярослав, ІПЗ-41/1\n")

; Task 5.1
(define (bisection function a b)
  (define m (/ (+ a b) 2))
      (if (not (= (function m) 0))
          (if (< (* (function a) (function m)) 0) ; умова вибору однієї з половин [a,(a+b)/2] і [(a+b)/2,b]
              (bisection function a m)  ; [a,(a+b)/2]
              (bisection function m b)) ; [(a+b)/2,b]
          m
          ))

(define EPS 1e-3) 

(define (newtons f df x0)
  (define xk (- x0 (/ (f x0) (df x0)))) ; обрахунок x[k+1] 
      (if (>= (abs (f xk)) EPS) ; зазначена процедура виконується до тих пір, поки |f(x[i])| > epsilon
          (newtons f df xk)
           xk))

(display "Завдання 5.1\n")
(display "x(метод бісекцій):\t")
(bisection (lambda (x) (+ (- (cos x) x) 5.)) 2. 7.)
(display "x(метод Ньютона):\t")
(newtons (lambda (x) (+ (- (cos x) x) 5.)) (lambda (x) (+ (-(sin x)) -1.)) 2.)

; Task 5.2
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (trapezium f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x h))
  (* 0.5 h (+ (f a) (* 2 (sum f (+ a h) next (- b h))) (f b)))) ; формула трапецій

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x (* 2 h)))
  (* (/ h 3)
     (+ (f a)
        (* 4 (sum f (+ a h) next (- b h)))
        (* 2 (sum f (+ a (* 2 h)) next (- b (* 2 h))))
        (f b)))) ; формула Сімпсона

(display "Завдання 5.2\n")
(display "Результат за формулою Симпсона:\t")
(simpson (lambda (x) (sin (+ (+ (* x x) x) 1))) 0 1 10)
(display "Результат за формулою трапецій:\t")
(trapezium (lambda (x) (sin (+ (+ (* x x) x) 1))) 0 1 10)

