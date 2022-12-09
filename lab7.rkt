#lang racket

;процедура створення списку пар: ( слово . частота повторення в тексті)
(define (word-frequencies string)
  (define (id x) x)
  (define (frequency-pair strings) (cons (car strings) (length strings)))
  (map frequency-pair (group-by id (string-split string))))  ;групування слів за їхніми же значеннями


(define file1 "/Users/denysliashenko/Desktop/FIT/4_1/FUNC/lab7/file1.txt")  ;записуємо шлях до файлу з текстом
(define file2 "/Users/denysliashenko/Desktop/FIT/4_1/FUNC/lab7/file2.txt")  ;записуємо шлях до файлу з результатом


;для запису в текстовий файл n рядків тексту
(display "Вкажіть кількість рядків тексту(n): ")  ;вказуємо кількість рядків нашого тексту
(define n (string->number (read-line (current-input-port) 'any)))  ;конвертуємо вказане n у число
(define lines '())  ;створюємо порожній масив lines для запису рядків з клавіатури

(for ([i n])  ;ітеративний цикл для запису тексту від 1 до n в створений масив
  (begin
    (printf "Введіть текст рядка номер ~a: " (+ i 1))
    (set! lines (append lines (list (read-line (current-input-port) 'any))))  ;додати до масиву новий введений рядок
  )
)

(let ((port (open-output-file file1)))  ;відкриваємо порт для запису файлу з текстом
  (for ([i n])  ;ітеративний цикл для запису кожного рядка в файл
    (write (list-ref lines i) port)  ;на кожній ітерації додати рядок
  )
  (close-output-port port)  ;закрити порт
)


;для зчитування рядків із створеного програмою файлу, та виводу їх на екран.
(newline)
(display "Заданий Вами текст: \n")
(define text-from-file '())  ;створення пустого списку для зчитування тексту з файлу
(define in (open-input-file file1))  ;створення порту для зчитування
(for ([i n])  ;ітеративний цикл зчитування n раз
  (begin
    (define current-line (read in))  ;считати рядок на ітерації
    (display current-line)  ;показати його
    (set! text-from-file (append text-from-file (list current-line)))  ;додати в список text-from-file
    (display "\n")
  )
)
(close-input-port in)  ;закриваємо порт


;виклик процедури створення пар та відображення результату
(newline)
(define result (word-frequencies (string-join text-from-file " ")))    ;присвоєння в result результату виконання процедури word-frequencies після передачі їй тексту з файлу
(display "Побудований список пар: (слово . частота повторення в тексті:) \n")
(display result)  ;відображення результату


;запис побудованих пар в файл
(let ((port (open-output-file file2)))  ;відкриваємо порт для запису результату в другий файл
  (write result port)  ;запис
  (close-output-port port)  ;закриваємо порт
)


