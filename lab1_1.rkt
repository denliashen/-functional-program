#lang racket

(define row 0)   ;лічільники
(define element-in-row 0)
(define depth 0)

(define (pascal x y)  ;процедура обчислення значення елемента в трикутнику за розположенням
  (set! depth (+ depth 1))  ;підраховуємо глибину рекурсії
    (cond ((or (<= x 0) (<= y 0) (< x y )) 0)
          ((or (= 1 y) (= x y) ) 1)
          (else (+ (pascal (- x 1) y) (pascal (- x 1) (- y 1))))))


(define (row-elements a) ;процедура яка відповідає за побудову елементів стовпичка
  (if (= a 0) ; якщо == 0, то стовпчик добудован
    (begin
      (set! element-in-row 0)) ;скидання лічільника
    (begin   ;інакше
      (display " ")
      (set! element-in-row (+ element-in-row 1))  ;значення номеру елементу в рядку
      (display (pascal row element-in-row)) ;відображаєм, передаємо номер рядка та номер елемента в рядку в pascal для обрахунку значення
      (row-elements (- a 1)))))


(define (triangle n)    ;процедура яка відповідає за побудову трикутника
  (if (= n 0) ; якщо == 0, то трикутник побудован
    (begin
      (newline)
      (newline)
      (display "Pascal's triangle is done!")
      (newline)
      (display "Recursion level:")
      (display depth)) ;відображення глибини рекурсії
    (begin    ;інакше
      (newline)
      (set! row (+ row 1)) ;значення номеру рядка
      (row-elements row) ;передаємо номер рядка в row-elements для визначення елементів в рядку
      (triangle (- n 1))))) 

(display "Enter value:")
(define value(read))  ;Зчитуємо значення
(triangle value)  ;Передаємо значення в triangle



