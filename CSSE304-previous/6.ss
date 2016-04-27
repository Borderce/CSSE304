;Bo Peng Assignment 6

;Problem 1
(define curry2 (lambda (func)
	(lambda (first)
		(lambda (second)
			(func first second)))))
			
;Problem 2
(define curried-compose (lambda (func1)
	(lambda (func2)
		(lambda (ls)
			(func1(func2 ls))))))
			
;Problem 3
(define funcs-apply (lambda (funcs obj)
	(if (null? funcs) 
		obj 
		(funcs-apply  
			(reverse(cdr(reverse funcs))) 
			((car(reverse funcs)) obj) ))
	))

(define compose (lambda list-of-functions
	(lambda (obj)
		(funcs-apply list-of-functions obj))))

;Problem 4
(define make-list-helper (lambda (num obj result)
	(if (zero? num) 
		result 
		(make-list-helper (- num 1) obj (append result (list obj))) )))
	
(define make-list-c (lambda (num)
	(lambda (obj)
		(make-list-helper num obj '()))))
		
;Problem 5
(define every-first-from-list (lambda (l)
	(if (null? (cdr l)) 
		(list(car(car l))) 
		(append (list (car(car l))) (every-first-from-list (cdr l)))  ) ))
		
(define every-last-from-list (lambda (l)
	(if (null? (cdr l)) 
		(cdr(car l)) 
		(append  (cdr(car l)) (every-last-from-list (cdr l)))  ) ))
		
	
(define let->application (lambda (ls)
	(cons
		(list 'lambda (if (null? (cadr ls)) 
							'() 
							(every-first-from-list (cadr ls)))
						(caddr ls))  
		(if (null? (cadr ls)) 
			'() 
			(every-last-from-list (cadr ls)) ) )))

;Problem 6
(define recursive-let (lambda (ls result)
	(if (null? ls) 
		result 
		(recursive-let (cdr ls)  (append (list 'let (list (car ls))) (list result))  ))))
	
(define let*->let (lambda (ls)
	(if  (<= 3 (length ls)) 
		(recursive-let (reverse (cadr ls)) (caddr ls)) 
		'())))

;Problem 7
(define filter-helper (lambda (func ls result)
	(if (null? ls) result (if (func (car ls))  
		(filter-helper func (cdr ls)(append result (list(car ls)))) 
		(filter-helper func (cdr ls) result )))))
		
(define filter-in (lambda (func ls)
	(filter-helper func ls '())))
	
;Problem 8
(define filter-out-helper (lambda (func ls result)
	(if (null? ls) result (if (not(func (car ls)))  
		(filter-out-helper func (cdr ls)(append result (list(car ls)))) 
		(filter-out-helper func (cdr ls) result )))))
		
(define filter-out (lambda (func ls)
	(filter-out-helper func ls '())))
	
;Problem 9
(define sort-list-of-symbols (lambda (ls)
	(map string->symbol(sort string<?(map symbol->string ls)))))
	
;Problem 10
(define invert (lambda (ls)
	(map reverse ls)))

;Problem 11
(define vector-check (lambda (pred vec num)
	(if (eqv? num (vector-length vec)) 
		#f 	
		(if (pred (vector-ref vec num)) 
			num 
			(vector-check pred vec (+ num 1))))
	))

(define vector-index (lambda (pred vec)
	(vector-check pred vec 0)))
