#lang racket

(define from 0)  ; початкове значення інтервалу
(define to 3.14)  ; кінцеве значення інтервалу


(define (Sipmsons x0 x02h f)  ; процедура чисельного інтегрування функції за методом Сімпсона
    (let ((h (/ ( - x02h x0) 2)))  ; h = ( (x0 + 2h) - x0 ) / 2
    (* (/ h 3) ( + (+ (f x0) (* 4 (f (+ x0 h)))) (f (+ x0 (* 2 h)))))))  ; формула Сімпсона : (h / 3) * ( f(x0) + 4f(x0 + h) + f(x0 + 2h) )


(define result_Simpson (Sipmsons from to (lambda(x)  ;  присвоєння результату виклику процедура чисельного інтегрування функції, з передачею інтервалу та функції, в result_Simpson
                           (sin (* 2 (cos x))))))

(display "Simpson: ")
(display result_Simpson) ; відображення результату  
(newline)


(define (rectangle a b n)  ; процедура чисельного інтегрування функції за методом середніх прямокутників
  (let computing ((result 0) (h (/ (- b a) n)) (x (/ (+ a b) 2)))  ;  (b - a)/n from i=1 to n f((x_i-1 + x_i)/2)
    (if (< b x) ; перевірка, виконання в межах визначенного інтервалу
        result  ; якщо так - відображення результату  
        (computing (+ result (* h (sin (* 2 (cos x))))) h (+ x h)))))  ; якщо ні - продовження


(display "Rectangle: ")
(display (rectangle from to 1))  ; відображення результату  


(newline)
(display "Difference: ")
(display (- result_Simpson (rectangle from to 1)))   ; різниця результатів