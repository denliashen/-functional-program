#lang racket

(define (Main n)
      (sqrt (+ (* 2 (- n 1)) (sqrt(* 2 n)))) )    ; незмінна частина виразу

(define (Expression n)   ; процедура для обчислення
  (define val (* n 2))    ; визначенння значення першого кореню
  (define result 0)    ; для підсумування результату на кожній ітерації
  (define (step i)  ; рекурсія
    (if (> i n)     ; умова для завершення
        0
       (begin
         (step (+ i 1))  ; наступна ітерація
         (set! result (sqrt (+ val (Main n))) )  ; обчислення виразу на кожній ітерації
         (set! val (- val 2))  ; зміна значення під корнем
       ))
    )
  (step 1)    ; запуск рекурсії
  (display result)  ; відображення результату
  )

(display "n: ")
(define value(read))
(display "result: ")
(Expression value)
