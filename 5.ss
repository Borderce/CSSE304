;Bo Peng Assignment 5

;Problem 1
(define interval-intersects?
	(lambda (1st 2nd) (cond
						[(or (null? 1st) (null? 2nd)) #f]
						[(<= (car 1st) (cadr 2nd)) (<= (car 2nd)(cadr 1st))] 
						[else (>= (car 2nd) (cadr 1st))])))
							
(define interval-union
	(lambda (1st 2nd) (if 
						(interval-intersects? 1st 2nd) 
						(list (list (min (car 1st) (car 2nd)) (max (cadr 1st) (cadr 2nd)))) 
						(if (or (null? 1st)(null? 2nd)) (append 1st 2nd) (list 1st 2nd)))))

(define minimize-interval-list-a
	(lambda (ele lst ind)
		(if 
			(null? lst)
			(if 
				ind 
				'() 
				(list ele))
			(if 
				(interval-intersects? (car lst) ele) 
				(append (interval-union (car lst) ele) (minimize-interval-list-a ele (cdr lst) #t)) 
				(append (list (car lst))(minimize-interval-list-a ele (cdr lst) ind))
			))))
		
(define minimize-interval-list-b
	(lambda (lst num) 
		(if 
			(equal? 0 num) 
			lst 
			(minimize-interval-list-b (minimize-interval-list-a (car lst) (cdr lst) #f) (- num 1)) 
		)))
		
(define minimize-interval-list 
	(lambda (lst)
		(minimize-interval-list-b lst (length lst))))
		
;Problem 2
(define exists? 
	(lambda (pred ls)
		(if 
			(null? ls) 
			#f 
			(or (pred (car ls)) (exists? pred (cdr ls)) ))))
			
;Problem 3
(define list-index (lambda (pred ls)
	(if (exists? pred ls) 
		(if (pred (car ls)) 
			0 
			(+ 1 (list-index pred (cdr ls)))) 
		#f)))
		
;Problem 4
(define pascal-line (lambda (n)
	(if (>= n 0) 
		(if (eqv? 0 n) 
			'(1) 
			(upper-line(pascal-line (- n 1))) )  
		'())))
(define upper-line (lambda (l)
	(map + (append l '(0)) (append '(0) l)) ))
		
(define pascal-triangle (lambda (n)
	(if (>= n 0) 
		(append(list(pascal-line n)) (pascal-triangle (- n 1)) )
		'()) ))
				
;Problem 5
(define sub-product (lambda (e l)
	(if (null? (cdr l)) 
		(list(list e (car l)))    
		( cons (list e (car l)) (sub-product e (cdr l)) ) )))

(define product (lambda (l1 l2)
	(if (or (null? l1) (null? l2)) 
		'() 
		(if (null? (cdr l1)) 
			(sub-product (car l1) l2)  
			(append (sub-product (car l1) l2) (product (cdr l1) l2))))
	 ))

;Problem 6
(define max-edges (lambda (n)
	(if (< n 2) 0 (+ (- n 1) (max-edges(- n 1))))))

;Problem 7
(define eq-helper-2
	(lambda (ls) (if 
					(null? ls)
					#t
					(and (car ls)(eq-helper-2 (cdr ls)))
					)))

(define eq-helper
	(lambda (ele l)  
		(map not 
			(map (lambda (x) (equal? ele x)) l))))

(define set?
	(lambda (l) (if 
					(or (null? l) (null? (cdr l)) )
					#t
					(if 
						(eq-helper-2(eq-helper (car l) (cdr l)))
						(set? (cdr l))
						#f
					))))	
				
(define all-equal? 
	(lambda (lst) 
		(if 
			(null? (cdr lst)) 
			#t
			(and (equal? (car lst) (cadr lst)) (all-equal? (cdr lst))))))
				
(define complete?
	(lambda (lst)
		(if 
			(null? lst) 
			#t 
			(if 
				(and (set? (map car lst)) (set? (map cadr lst)))
				(if 
					(all-equal? (map length (map cadr lst)))
					(if 
						(equal? (length (map car lst)) (+ (length (car (map cadr lst))) 1))
						#t
						#f)
					#f)
				#f
			))))	

;Problem 8
(define complete-sub-list (lambda (e l result)
	(if (null? l) 
		(list e result) 
		(if (eqv? e (car l)) 
			(complete-sub-list e (cdr l) result) 
			(complete-sub-list e (cdr l) (append result (list(car l))))))))

(define bind-sub-list (lambda (temp l result)
	 (if (null? temp) 
		result 
		(bind-sub-list 
			(cdr temp) 
			l 
			(append result (list(complete-sub-list (car temp) l '())))  )))  )
(define complete (lambda (l)
	(if (null? l) '() (bind-sub-list l l '()) )))
	
;Problem 9
(define replace-helper (lambda (old new ls result) 
	(if (null? ls) result 
		(if (eqv? (car ls) old) 
			(replace-helper old new (cdr ls)(append result (list new)))  
			(replace-helper old new (cdr ls)(append result (list (car ls))))))
	))
(define replace (lambda (old new ls)
	(replace-helper old new ls '())))
	
;Problem 10
(define remove-helper (lambda (e ls result i)
	(if (null? ls) 
		result 
		(if (eqv? e (car ls)) 
			(if i 
				(remove-helper e (cdr ls) (append result (list(car ls))) i) 
				(remove-helper e (cdr ls) result #t) ) 
			(remove-helper e (cdr ls) (append result (list(car ls))) i)))
	))
	
(define remove-first (lambda (e ls)
	(remove-helper e ls '() #f)))

;Problem 11
(define remove-last (lambda (e ls)
	(reverse(remove-helper e (reverse ls) '() #f))))	