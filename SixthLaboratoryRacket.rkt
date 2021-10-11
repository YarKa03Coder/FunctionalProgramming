#lang racket

(display "Лаб6, завдання 5.1-5.2, Вакулко Ярослав, ІПЗ-41/1\n")

; Task 5.1

(define (vector-max-negative v)
  (apply max
         (vector->list
          (vector-filter negative? v)
         )
  )
)

(define (vector-min-positive v)
  (apply min
         (vector->list
          (vector-filter positive? v)
         )
  )
)

(define (find-index user-vector value index)
  (if (> index (vector-length user-vector))
      -1
      (if (= (vector-ref user-vector index) value)
          index
          (find-index user-vector value (+ index 1))
      )
  )
)

(display "Task 5.1")
(newline)

(define user-vector (list->vector (list -3 2 -4 100 1 5 -10 -2 11 4 -7 9 6)))
(display (~a "user-vector є вектором: " (vector? user-vector))) ; перевірка чи це дійсно вектор
(newline)
(display (~a "Користувацький вектор: " user-vector))
(newline)
(define min-positive (vector-min-positive user-vector))
(define max-negative (vector-max-negative user-vector))
(display (~a "Найменше додатне значення: " min-positive "\n" "Найбільше від'ємне значення: " max-negative))
(newline)
(display (~a "Індекс найменшого додатнього значення: " (find-index user-vector min-positive 0) "\n"))
(display (~a "Індекс найбільш від'ємного значення: " (find-index user-vector max-negative 0) "\n"))

; Task 5.2
(display "Task 5.2")
(newline)

(define (merge-queues first-queue second-queue)
  (if (and (null? first-queue) (null? second-queue))
      '()
      (if (null? first-queue)
            (append (list (car second-queue))
                    (merge-queues '() (cdr second-queue))) ; додаємо елемент лише з другої черги
            
            (if (null? second-queue)
                     (append (list (car first-queue))
                             (merge-queues (cdr first-queue) '())) ; додаємо елемент лише з першої черги
                     (append (list (car first-queue) (car second-queue))
                             (merge-queues (cdr first-queue) (cdr second-queue))) ; додаємо елементи з обох черг
            )
      )
  )
)

(define first-queue (list 1 2 3 4 5 6)) ; перша черга
(define second-queue (list 7 8 9 10 11 12)) ; друга черга

(display (~a "Перша черга: " first-queue "\n"))
(display (~a "Друга черга: " second-queue "\n"))
(define third-queue (merge-queues first-queue second-queue)) ; результуюча об'єднана третя черга
(display (~a "Результуюча черга: " third-queue))