;Bo Peng

;Problem 4
(define sorted2? (lambda (lst)
	(apply < lst)))
	
;Problem 5
(define all-=? (lambda (ls)
	(if (null? ls)
		#t
		(if  
			(number? (car ls))
			(if (null? (cdr ls))
				#t
				(if (number? (cadr ls))
					(if (eq? (car ls) (cadr ls))
						(all-=? (cdr ls))
						#f)
					#f))
			#f))))
			
;Problem 6
(define ax-path-from-root-to-leaf (lambda (lst)
	(cadr max-path-from-root-to-leaf-h (lst))))

(define max-path-from-root-to-leaf-h 
	(lambda (ls)
		(cond 
			[(null? ls) '(0 0)]
			[(and (list? (cadr ls)) (list? (caddr ls)))
				(filter (car ls) (max-path-from-root-to-leaf-h (cadr ls)) (max-path-from-root-to-leaf-h (caddr ls) ))]
			[(and (list? (cadr ls))(number? (caddr ls)))(filter-r (car ls) (caddr ls) (max-path-from-root-to-leaf-h (cadr ls)))]
			[(and (number? (cadr ls)) (list? (caddr ls)))(filter-l (car ls) (cadr ls) (max-path-from-root-to-leaf-h (caddr ls)))]
			[else (list 
						(+ (car ls)(cadr ls) (caddr ls))
						(+ (car ls)(cadr ls) (caddr ls)))]
			)))
		
		
(define filter
	(lambda (mid llst rlst)
		(cond 
			[(> (cadr llst)(cadr rlst)) 
				(if (> (+ mid (car llst) (car rlst)) (cadr llst))
					(list (+ mid (car llst) (car rlst)) (+ mid (car llst) (car rlst)))
					(list (+ mid (car llst) (car rlst)) (cadr llst)))]
			[(< (cadr llst) (cadr rlst))
				(if (> (+ mid (car llst) (car rlst)) (cadr rlst)) 
					(list (+ mid (car llst) (car rlst)) (+ mid (car llst) (car rlst))) 
					(list (+ mid (car llst) (car rlst)) (cadr rlst)))]
			[else 
				(if (> (+ mid (car llst) (car rlst)) (cadr rlst))
					(list (+ mid (car llst) (car rlst)) (+ mid (car llst) (car rlst)))
					(list (+ mid (car llst) (car rlst)) (cadr llst)))]
			)))		
		
(define filter-l
	(lambda (mid num lst)
		(cond 
			[(< num 0) (list (+ mid (car lst) num) (cadr lst))]
			[(> (+ mid (car lst) num) (cadr lst)) (list (+ (car lst) num mid) (+ (car lst) num mid))]
			[else (list (+ (car lst) num mid) (+ (car lst) num mid))]
			)))

(define filter-r
	(lambda (mid num lst)
		(cond 
			[(< num 0) (list (+ mid (car lst) num) (cadr lst))]
			[(> (+ mid (car lst) num) (cadr lst)) (list (+ (car lst) num mid) (+ (car lst) num mid))]
			[else (list (+ (car lst) num mid) (cadr lst))]
			)))		

		