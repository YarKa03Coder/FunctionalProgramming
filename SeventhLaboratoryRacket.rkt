#lang racket

(display "Лаб7, завдання 5.1, Вакулко Ярослав, ІПЗ-41/1\n")

; Task 5.1
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))

(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
      '()  
      (begin(displayln line)
      (append (list line) (next-line-it file))) ; зберігаємо порядково записи файлу в список
     )
   )
)

(define (capitalize-first-letter str)
  (cond
    [(non-empty-string? str)
     (define first-letter-str
       (substring str 0 1))
     (define rest-str
       (substring str 1 (string-length str)))
     (string-append (string-upcase first-letter-str)
                    rest-str)]
    [else
     ""]))

(define (capitalize-sentences-of-list user-list)
  (if (null? user-list)
    '()
    (append (list (capitalize-first-letter (car user-list))) (capitalize-sentences-of-list (cdr user-list))) 
 ))


(define words-list (list
                    "when I find myself in times of trouble."
                    "mother Mary comes to me."
                    "speaking wise, blessing words."
                    "i thank my mother for her help."
                    "i'm moving forward."
                   )
) ; текст, що заданий програмно, за допомогою списків

(list->file words-list "E:\\FourthCourse\\Crossplatforming\\SEVEN_INPUT.txt") ; записуємо текст до файлу
(displayln "Вміст файлу:")
(define words-list-updated (call-with-input-file "E:\\FourthCourse\\Crossplatforming\\SEVEN_INPUT.txt" next-line-it)) ; виводимо його вміст
(set! words-list-updated (capitalize-sentences-of-list words-list-updated)); Робимо перші літери в верхньому регістрі
(list->file words-list-updated "E:\\FourthCourse\\FunctionalProgramming\\SEVEN_RESULT.txt") ; записуємо модифікований текст до результуючого файлу
(display "Успішно записано модифікований файл")