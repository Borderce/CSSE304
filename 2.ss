;Bo Peng Assignment 2
;Problem 1
(define fact
	(lambda (num) (if 
					(eq? num 0) 
					1 
					(* (fact (- num 1)) num))))

;Problem 1b
(define choose
	(lambda (1st 2nd)
		(/ (fact 1st) (* (fact 2nd) (fact (- 1st 2nd))))))

;Problem 2
(define range
	(lambda (f l) (if
					(< f l)
					(cons f (range (+ 1 f) l))
					'())))

;Problem 3
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

;Program 4
(define sum-of-squares 
	(lambda (l) 
		(if 
			(null? l)
			0
			(+ (* (car l) (car l)) (sum-of-squares (cdr l)))
		)))

;Program 5
(define make-vec-from-points 
	(lambda (p1 p2) (map - p2 p1)))

;Program 6
(define dot-product
	(lambda (p1 p2) (apply + (map * p1 p2))))
	
;Program 7
(define square 
	(lambda (x) (* x x)))

(define vec-length
	(lambda (ls) 
		(sqrt(+ (square (car ls)) (square (cadr ls)) (square (caddr ls))))))
		
;Program 8
(define distance 
	(lambda (1st 2nd)
		(sqrt (apply + (map square (map - 2nd 1st))))))

;Program 9
(define cross-helper
	(lambda (ul ur ll lr)
		(- (* ul lr) (* ur ll))))
		
(define cross-product
	(lambda (1st 2nd)
		(list 
			(cross-helper (cadr 1st) (caddr 1st) (cadr 2nd) (caddr 2nd)) 
			(- 0 (cross-helper (car 1st) (caddr 1st) (car 2nd) (caddr 2nd)))
			(cross-helper (car 1st) (cadr 1st) (car 2nd) (cadr 2nd))
		)))
		
;Program 10
(define parallel?
	(lambda (1st 2nd)
		(equal? 0 (sum-of-squares(cross-product 1st 2nd)))))

;Program 11
(define collinear-helper
	(lambda (lst)
		(and (equal? (car lst) (cadr lst)) (equal? (cadr lst) (caddr lst)))))
	
(define collinear?
	(lambda (1st 2nd 3rd)
		(collinear-helper (map /(map - 2nd 1st) (map - 3rd 2nd)))))