#lang racket
(define (main)  ;головна функція
  (define digit (read)) ;зчитування з клавіатури
  (if (integer? digit) ;перевірка на ціле число
    (begin   ;якщо так, то
      (if(= digit 0) ;перевірка чи ==0
        (display "") ;кінець
        (begin  ;якщо так, то
          (if (odd? digit) ;перевірка чи непарне
            (begin
              (display digit) ;відображення результату
              (display " - odd digit")
              (newline)
              (main)) ;продовження зчитування
            (main))))) ;продовження зчитування
      (begin
        (display "Enter:")
        (main)))) ;продовження зчитування


(main) ;старт програми