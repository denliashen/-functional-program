#lang racket

(define a -10)  ;  значення [а, б] інтервалу
(define b 10)
(define acc 0.0001)  ;  значення точності для методу Ньютона
(define step 0.01)  ;  значення кроку для методу перебору


(define (func x) (- (+ (expt x 3) 6.0) (* 4.0 x)))  ;  нелінійне рівняння
(define (derivative x) (- (* 3.0 (expt x 2)) 4.0))  ;  похідна рівняння

  ; ------- метод Ньютона
(define x1 0)
(define i a)  ; лічильник
(define result 0)   ; для збереження остаточного результату методом Ньютона
(define (newton x0)  ;  рекурсивна процедура обчислення методом Ньютона
        (set! x1 (- x0 (/ (func x0) (derivative x0)))) ; присвоєння нової точки приближення х1, за формулою Xn+1 = Xn -( f(Xn) / f(X'n) )
        (set! i (+ i 1))  ;  збільшення лічільника
        (if (< i b)  ; перша перевірка, виконання в межах визначенного інтервалу [а, б]
          (if (< (abs (- x1 x0)) acc)  ; друга перевірка, чи задовольняє визначена точка приближення умові Xn+1 - Xn < accuracy
            (set! result x0)  ; якщо так - запис остаточного результату
            (newton x1))   ; якщо ні - продовження
          0))

(display "Newton method: ")
(newton a)  ; виклик рекурсивною процедури Ньютона з передачою а
result ; відображення результату


  ; ------- метод перебору
(define (check x)  ; процедура для визначення знаку переданного значення
  (cond 
    [(negative? x) -1]   ; якщо значення негативне, повертає -1
    [(positive? x)  1]   ; якщо значення позитивне, повертає 1
    [else 0]))   ; інакше, повертає 0


(set! i a)   ; присвоєння значення а лічільнику
(define ex 0)  ; для збереження Хn-1
(define result2 0)  ; для збереження остаточного результату методом перебору
(define (enumeration x0)  ;  рекурсивна процедура обчислення методом перебору
        (set! ex x1)  ;  присвоєння поточного значення попередньому
        (set! x1 (func x0))  ; визначення нового значення Хn
        (set! i (+ i step))  ;  збільшення лічільника на зазначений крок
        (if (< i b)  ; перша перевірка, виконання в межах визначенного інтервалу [а, б]            
          (if (= (check x1) (check ex))  ; друга перевірка, чи відбулась зміна знаку між поточним і попереднім значенням
            (begin  ; якщо ні - рекурсивний виклик
              (set! x0 (+ x0 step))  ; збільшення значення X на зазначений крок
              (enumeration x0))  ; продовження з новим значення Х
            (set! result2 (- x0 (/ step 2)))) ; якщо так - запис остаточного результату
          0))
  
(enumeration a)   ; виклик рекурсивною процедури перебору з передачою а
(display "Enumeration method: ")
result2  ; відображення результату

(display "Difference: ")
(- result result2)  ; різниця результатів  
