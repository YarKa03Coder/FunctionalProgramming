#lang racket

(display "Лаб5, завдання 5.1-5.2, Вакулко Ярослав, ІПЗ-41/1\n")

; Task 5.1
(define (numerator x) (car x)) ; чисельник
(define (denominator x) (cdr x)) ; знаменник
(define (create-fraction n d) (cons n d)); створення пари

(define(print-fraction x) ; друк пари 
  (display (numerator x))
  (display "/")
  (display (denominator x)))

(define (substract-fraction x y)
       (create-fraction (- (* (numerator x) (denominator y))
                             (* (numerator y) (denominator x)))
                          (* (denominator x) (denominator y))))

(define (divide-fraction x y)
        (create-fraction (* (numerator x) (denominator y))
                          (* (denominator x) (numerator y))))

(define one-third-of-distance (create-fraction 1 3))
(define half-of-distance (create-fraction 1 2))
(define task-const-value 2)

(display "Task 5.1")
(newline)

; друкуємо рівняння, яке необхідно розв'язати
(display "Сформоване рівняння згідно поставленої задачі: ")
(begin
  (display "(")(print-fraction one-third-of-distance)(display ")x") ; x - відстань між селами
  (display " = ")
  (display "(")(print-fraction half-of-distance)(display ")x") ; x - відстань між селами
  (display " - ")
  (display task-const-value)
)
(newline)

(define substract-value (substract-fraction one-third-of-distance half-of-distance))

; друкуємо результат після віднімання
(display "Результат після віднімання дробів: ")
(begin
  (display "(") (print-fraction substract-value)(display ")x") ; x - відстань між селами
  (display " = ")
  (display "-")
  (display task-const-value)
)
(newline)

(define x (divide-fraction (create-fraction (* task-const-value -1) 1) substract-value))

; виводимо знайдену відстань
(display "Знайдена відстань між селами: ")
(begin
  (display "x = ")
  (print-fraction x)
  (display " = ")
  (display (/ (numerator x) (denominator x)))
)
(newline)

; Task 5.2
(define (real-part z) (car z))
(define (imagine-part z) (cdr z))

(define (create-complex-value a b) (cons a b))

(define (create-complex-list input-list)
  (if (null? input-list)
    '()  
    (append (list
             (string->number (~a (real-part (car input-list)) "+" (imagine-part (car input-list)) "i"))
             )
            (create-complex-list (cdr input-list))))
)

(display "Task 5.2")
(newline)

; користувацький список комплексних чисел в алгебраїчній формі
(define complex-list (create-complex-list(list
                      (create-complex-value 2 7)
                      (create-complex-value 7 2)
                      (create-complex-value 3 4)
                      (create-complex-value 1 5)
                      (create-complex-value 9 8))))

(display "Користувацький список комплексних чисел (алгебраїчна форма): ")
(display complex-list)

(newline)
(display "Добуток всіх елементів: ")
(display (apply * complex-list))