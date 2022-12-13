#lang racket

;========= символьне диференціювання====
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (make-devide m1 m2) (list '/ m1 m2))
(define (sum? x) ; Чи це добуток
(and (pair? x) (eq? (car x) '+)))
(define (minus? x)
(and (pair? x) (eq? (car x) '-)))
(define (make-minus a1 a2) (list '- a1 a2)) ; продиференціювати операцію мінус
(define (addend s) (cadr s)); взяти перший аргумент 
(define (augend s) (caddr s)); взяти другий аргумент
(define (product? x)
(and (pair? x) (eq? (car x) '*)))
(define (devide? x)
(and (pair? x) (eq? (car x) '/)))
(define (multiplier p) (cadr p)) ; взяти перший аргумент 
(define (multiplicand p) (caddr p)); взяти другий аргумент

(define (euler? x)
(and (pair? x) (eq? (car x) 'euler)))
(define (make-euler a1 a2 ) (list 'exp a1) )

(define (pow? x)
(and (pair? x) (eq? (car x) 'pow)))
(define (make-pow a1 a2) (list `* (list 'n) (list 'expt a1 (list `- 'n 1) ) )  )

(define (lg? x)
(and (pair? x) (eq? (car x) 'lg)))
(define (make-lg a1 a2) (list `/ 1 (list `* a1 (list 'log 10) ) )  )

(define (deriv exp var)
(cond ((number? exp) 0)
((variable? exp)
(if (same-variable? exp var) 1 0))
((sum? exp)
(make-sum (deriv (addend exp) var)
(deriv (augend exp) var)))

((minus? exp)
(make-minus (deriv (addend exp) var)
(deriv (augend exp) var))) 

((product? exp)
(make-sum
(make-product (multiplier exp)
(deriv (multiplicand exp) var))
(make-product (deriv (multiplier exp) var)
(multiplicand exp))))
((devide? exp)
( make-devide 
(make-minus
(make-product (deriv (multiplier exp) var)
(multiplicand exp)) 
(make-product (multiplier exp)
(deriv (multiplicand exp) var))
) (make-product (multiplicand exp) (multiplicand exp) ) ) )
((euler? exp)
(make-euler (addend exp) (deriv (addend exp) var) ) 
)
((pow? exp)
(make-pow (addend exp) (deriv (addend exp) var) ) 
)
((lg? exp)
(make-lg (addend exp) (deriv (addend exp) var) ) 
)
(else
(display "unknown expression type - DERIV" ))))
(deriv '(euler x) 'x )
(deriv '(pow x n) 'x )
(deriv '(lg x) 'x )
(deriv '5 'x)