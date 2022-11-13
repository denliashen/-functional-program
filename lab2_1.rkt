#lang racket

(define (y_ver x)    ;для обчислення похибки
  (cond
    ((or (>= x -2) (< x 0))
        (+ (expt (log (+ 1 x)) 2) (* 2 (log (+ 1 x)))) 
    )
    ((or (>= x 0) (<= x 3))
        (log (+ 1 x))
    )
  )
)

;---------------------------------

(define (TaylorForAbsSmallX x iter)    ; розкладання ln(1+x) для |x| < 1
  (define result 0)    ; для сумування результату
  (define (TaylorForAbsSmallX_Iter k)
    (if(= k iter)    ; умова зупинки
       (- 0 result)  ; відображення результату
       (begin
         (set! result (+ result (/ (expt (- x) k) k)))  ; додавання розкладу в результат
         (TaylorForAbsSmallX_Iter (+ k 1))  ; рекурсивний виклик зі збільшенням k
       )    
    )
  )
  (TaylorForAbsSmallX_Iter 1) ; запуск рекурсії
)


(define (TaylorForAbsBigX x iter)    ; розкладання ln(1+x) для |x| > 1
  (define result 0)    ; для сумування результату
  (define (TaylorForAbsBigX_Iter k)
    (if(= k iter)    ; умова зупинки
       (- (log x) result)  ; відображення результату
       (begin
         (set! result (+ result (/ (expt (- 1) k) (* k (expt x k)))))  ; додавання розкладу в результат
         (TaylorForAbsBigX_Iter (+ k 1))  ; рекурсивний виклик зі збільшенням k
       )    
    )
  )
  (TaylorForAbsBigX_Iter 1) ; запуск рекурсії
)


(define (Taylor x iter)    ;визначення яка саме функція розкладання необхідна в залежності від значення х
   (cond
     ((<= (abs x) 1)    ; якщо |x| < 1 то викликається TaylorForAbsSmallX
       (TaylorForAbsSmallX x iter))
     ((> (abs x) 1)    ; якщо |x| > 1 то викликається TaylorForAbsBigX
       (TaylorForAbsBigX x iter))
    )
)


(define (TaylorTop x iter)    ; для обчислення значення у при -2 <= x <= 0
  (+ (expt (Taylor x iter) 2) (* 2 (Taylor x iter))) )


(define (y x iter)    ; виклик необхідних функцій для обчислення в залежності від переданого значення х
  (cond
    ((or (>= x -2) (< x 0))
        (TaylorTop x iter)
    )
    ((or (>= x 0) (<= x 3))
        (Taylor x iter)
    )
  )
)

(define x_from -3)
(define x_to 3)
(define dx 0.5)


(define start   ; передається значення х в функцію у покроково від -3 до 3 з різнецею в 0.5
  (for/list ([x (in-inclusive-range x_from x_to dx)])
      (display (y x 10))
      (display " похибка: ")
      (display (- (y x 10) (y_ver x)) )
      (newline)))




