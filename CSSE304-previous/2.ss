;Bo Peng Assignment 2

;Program 1(a)
(define fact (lambda (n) 
	(if (= 0 n) 1 (* n (fact (- n 1))))))

;Program 1(b)
(define choose (lambda (n k)
	(/ (fact n) (* (fact k) (fact (- n k))))))

;Program 2
(define range (lambda (m n) 
	(if (< m n) (cons m (range(+ 1 m) n)) (list))))
	
;Program 3
(define duplicate? (lambda (l e)
	(if (null? l) #f (or (duplicate? (cdr l) e) (equal? e (car l))) ))) 

(define antiset? (lambda (l)
	(if (null? l) #f (or (duplicate? (cdr l) (car l)) (antiset? (cdr l))))))

(define set? (lambda (l)
	(not (antiset? l))))
	
;Program 4
(define square (lambda (n)
	(* n n)))

(define sum (lambda (l)
	(if (null? l) 0 (+ (car l) (sum (cdr l))))))
	
(define sum-of-squares (lambda (lon)
	(sum (map square lon))))
	
;Program 5
(define make-vec-from-points (lambda (p1 p2)
	(map - p2 p1)))
	
;Program 6
(define dot-product (lambda (v1 v2)
	(sum (map * v1 v2))))

;Program 7
(define vec-length (lambda (v) 
	(sqrt (dot-product v v))))

;Program 8
(define distance (lambda (v1 v2)
	(sqrt(sum(map square(map - v1 v2))))))

;Program 9
(define cross-product (lambda (u v)
	(list 
	(- (* (cadr u) (caddr v)) (* (caddr u)(cadr v))) 
	(- (* (caddr u) (car v)) (* (car u) (caddr v))) 
	(- (*(car u) (cadr v)) (*(cadr u) (car v))) )))

;Program 10
(define parallel? (lambda (v1 v2)
	 (eqv? 0 (sum(cross-product v1 v2))))) 
	 
;Program 11
(define mod0? (lambda (l1 l2)
	(or 
	(eqv? 0 (dot-product(map modulo l2 l1) (map modulo l2 l1)))
	(eqv? 0 (dot-product(map modulo l1 l2) (map modulo l1 l2))))))
(define collinear? (lambda (p1 p2 p3) 
	(mod0? (map - p1 p2) (map - p2 p3))))