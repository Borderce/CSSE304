;Assignment 1

;Problem 1
(define Fahrenheit->Celsius 
	(lambda (num) (* (- num 32) (/ 5 9)) )) 

;Problem 2
(define interval-contains? 
	(lambda (int num) (and (<= num (cadr int)) (>= num (car int)))))
	
;Problem 3
(define interval-intersects?
	(lambda (1st 2nd) (cond 
						[(<= (car 1st) (cadr 2nd)) (<= (car 2nd)(cadr 1st))] 
						[else (>= (car 2nd) (cadr 1st))])))
							
;Problem 4
(define interval-union
	(lambda (1st 2nd) (if 
						(interval-intersects? 1st 2nd) 
						(list (list (min (car 1st) (car 2nd)) (max (cadr 1st) (cadr 2nd)))) 
						(list 1st 2nd))))
;Problem 5
(define divisible-by-7?
	(lambda (num) (equal? 0 (modulo num 7))))
	
;Problem 6
(define ends-with-7?
	(lambda (num) (equal? 7 (modulo num 10))))

;Problem 7
(define 1st (lambda (lst) (car lst)))
(define 2nd (lambda (lst) (cadr lst)))
(define 3rd (lambda (lst) (caddr lst)))