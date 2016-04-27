;Bo Peng Assignment 3

;Problem 1
(define sum (lambda (l)
	(if (null? l) 0 (+ (car l) (sum (cdr l))))))
	
(define square (lambda (n)
	(* n n)))
	
(define distance (lambda (v1 v2)
	(sqrt(sum(map square(map - v1 v2))))))

(define get-nearest (lambda (p near list-of-points)
	(if (null? list-of-points) near (if (< 
	(distance p (car list-of-points)) 
	(distance p near) ) 
	(get-nearest p (car list-of-points) (cdr list-of-points)) 
	(get-nearest p near (cdr list-of-points))))))
	
(define nearest-point (lambda (p list-of-points)
	(if (null? list-of-points) '() (get-nearest p (car list-of-points) list-of-points ) )))

;Problem 2
(define contain? (lambda (e l)
	(if (null? l) #f (if (eqv? e (car l)) #t (or #f (contain? e (cdr l)))))))

(define union (lambda (l1 l2)
	(if (or (null? l1)(null? l2)) 
		(append l1 l2) 
		(if (contain? (car l2) l1) 
			(union l1 (cdr l2))
			(union (append l1 (list (car l2))) (cdr l2))) )))

;Problem 3
(define get-duplicate (lambda (l1 l2 result) 
	(if (null? l2) result 
	(if (contain? (car l2) l1)
	(get-duplicate l1 (cdr l2) (append result (list(car l2) )))
	(get-duplicate l1 (cdr l2) result)
		))
	))

(define intersection (lambda (l1 l2) 
	(get-duplicate l1 l2 '())))
	
;Problem 4
(define get-sub? (lambda (l1 l2)
	(if(null? l1) #t  (and(get-sub? (cdr l1) l2)(contain? (car l1) l2)) )))
	
(define subset? (lambda (l1 l2)
	(if (null? l1) #t (get-sub? l1 l2))))
	
;Problem 5
(define duplicate? (lambda (l e)
	(if (null? l) #f (or (duplicate? (cdr l) e) (equal? e (car l))) ))) 

(define antiset? (lambda (l)
	(if (null? l) #f  (or (duplicate? (cdr l) (car l)) (antiset? (cdr l))))))
	
(define length2? (lambda (l)
	(if (null? l) #t (if (eqv? 2 (length (car l))) (length2? (cdr l)) #f ))))

(define set? (lambda (l)
	(and (length2? l)(not (antiset? l)))))
	
(define relation? (lambda (l)
	(if (list? l) (if (andmap list? l) (if (null? l) #t (set? l)) #f)  #f)))
	
;Problem 6
(define get-domain (lambda (l result)
	(if(null? l) result 
	(if (duplicate? result (car(car l)) ) 
	(get-domain (cdr l) result) 
	(get-domain (cdr l) (append result (list(car(car l)))) 
	)))))

(define domain (lambda (l)
	(if (null? l) '() (get-domain l '()))))

;Problem 7
(define find-reflexive(lambda (l result)
	(if(null? l) result 
		(if  (duplicate? result (car(car l)))  
			(if (duplicate? result (cadr(car l))) 
				(find-reflexive (cdr l) result) 
				(find-reflexive (cdr l) (append result (list(cadr(car l))))))  
			(if (duplicate? result (cadr(car l))) 
				(find-reflexive (cdr l) (append result (list(car(car l))))) 
				(find-reflexive (cdr l) (append result (list(car(car l))) (list(cadr(car l))))))
				))))
(define get-reflexive (lambda (ref l)
	(if (null? ref) #t (if (duplicate? l (list(car ref)(car ref))) (get-reflexive (cdr ref) l) #f))))
	
(define reflexive? (lambda (l)
	(get-reflexive(find-reflexive l '()) l)))

;Problem 8
(define step-count (lambda (n result)
	(if (eqv? 1 n) 
		result 
		(if (eqv? 0 (modulo n 2)) 
			(step-count (/ n 2) (+ result 1)) 
			(step-count (+ 1(* 3 n)) (+ result 1) )))))

(define hailstone-step-count (lambda (n)
	(step-count n 0)))