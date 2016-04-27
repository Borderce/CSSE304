;Bo Peng Assignment 4

;Problem 1
(define set? (lambda (l)
	(if (null? l) 
		#t  
		(if (and(positive? (cadr(car l)))(symbol?(car(car l))))
			(set? (cdr l)) 
			#f)))) 	
	
	
(define duplicate? (lambda (l e)
	(if (null? l) 
		#f 
		(or 
			(duplicate? (cdr l) e) 
			(equal? e (car(car l))) ))))  

(define antiset? (lambda (l)
	(if (null? l) 
		#f  
		(or 
			(duplicate? (cdr l) (car(car l)))  
			(antiset? (cdr l)) ))))
	
(define length2? (lambda (l)
	(if (null? l) 
		#t 
		(if (and (list? (car l))(eqv? 2 (length (car l)))) 
			(length2? (cdr l)) 
			#f ))))


	
(define multi-set? (lambda (l)
	(if (null? l) 
		#t 
		(and 
			(length2? l)
			(and (set? l) (not (antiset? l)))))))

;Program 2
(define ms-size (lambda (l)
	(if (null? l) 0 (apply + (map cadr l)) )))

;Program 3
(define matrix-inner (lambda (m col)
	(if (eqv? 0 col) (car m) (matrix-inner (cdr m) (- col 1)))))
	
(define matrix-ref (lambda (m row col)
	(if (eqv? 0 row) 
		(if (eqv? 0 col) 
			(car(car m)) 
			(matrix-inner (car m) col)) 
		(matrix-ref (cdr m) (- row 1) col) )))

;Program 4
(define anti-duplicate? (lambda (l e)
	(if (null? l) 
		#t 
		(and 
			(anti-duplicate? (cdr l) e) 
			(equal? e (car l)) ))))
			
(define matrix? (lambda (obj)
	(if (list? obj) 
		(if (andmap list? obj) 
			(if (eqv? 0 (length(car obj))) 
				#f
				(if (anti-duplicate? (cdr (map length obj)) (car (map length obj)))
					#t
					#f)) 
			#f)
		#f )))

;Program 5
(define sub-list (lambda (l)
	(if (null? l) '() (append (list(car(car l))) (sub-list (cdr l) )) )))

(define new-list (lambda (l result)
	(if (null? l) result (new-list (cdr l) (append result (list(cdar l)) ) ) )))

(define not-valid? (lambda (l)
	(if (and(list? l)(not(null? l)))  
	(anti-duplicate? l '()) 
	#f )))
	
(define trans (lambda (obj result)
	(if (not-valid? obj) 
		result 
		(trans (new-list obj '()) ( append result (list(sub-list obj)) )))))

(define matrix-transpose (lambda (obj)
	(trans obj '())))
	
;Program 6
(define last-process (lambda (l result)
	(if (null? l) result (last-process (cdr l) (car l)))))

(define last (lambda (l) 
	(last-process l '())))

;Program 7
(define all-but-last (lambda (l)
	(if (null? (cdr l)) '() (append (list(car l)) (all-but-last (cdr l))) )))	
	
	
