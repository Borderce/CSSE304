;Bo Peng
;Problem 1
(define square 
	(lambda (x) (* x x)))

(define distance 
	(lambda (1st 2nd)
		(sqrt (apply + (map square (map - 2nd 1st))))))

(define nearest-point-helper
	(lambda (p ls) (map list (map (lambda (p2) (distance p p2)) ls) ls) ))

(define minimum-helper
	(lambda (ls) (cond 
						((null? (cdr ls)) (car ls))
						((< (car (car ls)) (car (minimum-helper (cdr ls)))) (car ls))
						(else (minimum-helper (cdr ls)))
						)))
	
(define nearest-point 
	(lambda (p ls) (cadr (minimum-helper(nearest-point-helper p ls)))))

;Problem 2
(define union
	(lambda (ls1 ls2) (if 
						(null? ls2)
						ls1
						(if 
							(member (car ls2) ls1)
							(union ls1 (cdr ls2))
							(union (cons (car ls2) ls1) (cdr ls2))
							)
						)))

;Problem 3
(define intersection-helper
	(lambda (ls1 ls2 r) (if 
							(null? ls2) 
							r 
							(if 
								(member (car ls2) ls1)
								(if 
									(member (car ls2) r)
									(intersection-helper ls1 (cdr ls2) r)
									(intersection-helper ls1 (cdr ls2) (cons (car ls2) r))
									)
								(intersection-helper ls1 (cdr ls2) r)
						)
						)))

(define intersection
	(lambda (ls1 ls2)
		(intersection-helper ls1 ls2 '())))

;Problem 4
(define subset?
	(lambda (ls1 ls2) (if 
						(null? ls1)
						#t
						(if 
							(member (car ls1) ls2)
							(subset? (cdr ls1) ls2)
							#f
							))))
				
;Program 5
(define all-length-2?
	(lambda (ls) (if 
					(null? ls)
					#t
					(if  
						(and (list? (car ls))(equal? 2 (length (car ls)))) 
						(all-length-2? (cdr ls)) 
						#f
					)
	)))

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

(define relation?
	(lambda (ls) (if 
					(list? ls) 
					(if 
						(all-length-2? ls) 
						(set? ls) 
						#f) 
					#f
					)))

;Problem 6
(define domain-helper
	(lambda (ls r) (if 
					(null? ls)
					r
					(if 
						(member (car (car ls)) r)
						(domain-helper (cdr ls) r)
						(domain-helper (cdr ls) (cons (car (car ls)) r))
						)
					)))

(define domain
	(lambda (ls) (domain-helper ls '())))

;Problem 7
(define range-helper
	(lambda (ls r) (if 
					(null? ls)
					r
					(if 
						(member (cadr (car ls)) r)
						(range-helper (cdr ls) r)
						(range-helper (cdr ls) (cons (cadr (car ls)) r))
						)
					)))
		
(define range
	(lambda (ls) (range-helper ls '())))
	
(define reflexive-helper
	(lambda (ls) 
		(union (range ls) (domain ls))
		))

(define combin-set
	(lambda (ls r) (if 
						(null? (cdr ls)) 
						(cons (list (car ls)(car ls)) r )
						(combin-set (cdr ls)(cons (list (car ls)(car ls)) r ))) 
						))
	
(define reflexive?
	(lambda (ls)
		(if 
			(null? ls)
			#t
			(subset? (combin-set (reflexive-helper ls) '()) ls)
		)))
		
;Problem 8
(define hailstone-step-count
	(lambda (num) (if 
					(equal? num 1) 
					0 
					(if 
						(equal? 0 (modulo num 2))
						(+ 1 (hailstone-step-count (/ num 2)))
						(+ 1 (hailstone-step-count  (+ 1 (* num 3))))
					))))