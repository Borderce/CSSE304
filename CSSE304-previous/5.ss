;Bo Peng Assignment 5

;Problem 1
(define (last-element l)
  (if (null? (cdr l)) (car l) (last-element (cdr l))))


(define interval-contains?
  (lambda (interval number) (if (and (not (< number (car interval))) (not (> number (last-element interval)))) '#t '#f)))

(define small?
  (lambda (i1 i2) (if (and
		       (< (car i1) (last-element i2))
		       (> (car i2)(last-element i1)))
		      '#t
		      '#f)))

(define big?
  (lambda (i1 i2) (if (and (> (car i1) (last-element i2)) (< (car i2)(last-element i1))) '#t '#f)))

(define interval-intersects?
  (lambda (i1 i2) (if (or (big? i1 i2) (small? i1 i2)) '#f '#t)))

(define new-interval
  (lambda (i1 i2) (list (list (min (car i1) (car i2)) (max (last-element i1)(last-element i2))))))

(define combine-interval
  (lambda (i1 i2) (list i1 i2)))

(define interval-union
  (lambda (i1 i2) (if (interval-intersects? i1 i2) (new-interval i1 i2) (combine-interval i1 i2))))
  
  
(define interval-list (lambda (e l) 
	(if (null? l) 
		e 
		(interval-list 
			(append (interval-union (car e) (car l)) (cdr e)) 
			(cdr l)))))
 
(define minimize-interval-list (lambda (l) 
	(if (null? l) '() (cons
		(car(interval-list (list(car l)) (cdr l))) 
		(minimize-interval-list (cdr(interval-list (list(car l)) (cdr l)))))
		)))

;Problem 2
(define exists? (lambda (pred ls) 
	 (if (null? ls) 
		#f 
		(if (pred (car ls)) 
			#t 
			(exists? pred (cdr ls))))  )) 

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
(define contain? (lambda (e l)
	(if (null? l) #f (if (eqv? e (car l)) #t (or #f (contain? e (cdr l)))))))

(define every-first-from-list (lambda (l)
	(if (null? (cdr l)) 
		(list(car(car l))) 
		(append (list (car(car l))) (every-first-from-list (cdr l)))  ) ))


(define duplicate? (lambda (e l)
	(if (null? l) 
		#f 
		(or 
			(duplicate? e (cdr l) ) 
			(equal? e (car l)) ))))
			
(define duplicate-element-in-list? (lambda (l)
	(if (null? l) 
		#f 
		(if (null?(cdr l)) 
			#f 
			(or (duplicate-element-in-list? (cdr l)) (duplicate? (car l) (cdr l))) ))))

	
(define equal-length-path? (lambda (l num)
	(if (null? l) #t (if (and(eqv? 
								(length(cadr(car l))) 
								(- num 1))
							(not (duplicate-element-in-list? (cadr(car l))))) 
						(if (duplicate? (car(car l)) (cadr(car l)) ) 
							#f 
							(equal-length-path? (cdr l) num)) 
						#f))))
						
(define complete? (lambda (l)
	(if (null? l) #t (if(duplicate-element-in-list?(every-first-from-list l)) 
		#f 
		(equal-length-path? l (length l))))
	))

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