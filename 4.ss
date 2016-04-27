;Assignemnt 4

;Problem 1
(define get-first (lambda (lst)
                    (cond [(null? lst) '()]
                          [else (cons (car (car lst)) (get-first (cdr lst)))])))

(define check-duplicate (lambda (ele lst)
                          (cond [(null? lst) #f]
                                [(equal? ele (car lst)) #t]
                                [else (check-duplicate ele (cdr lst))])))

(define multi-set-helper (lambda (lst checked)
                     (cond [(null? lst) #t]
                           [(list? (car lst)) (if 
                                                  (or (check-duplicate (car (car lst))(get-first checked)) (< (cadr (car lst)) 0))
                                                  #f
                                                  (multi-set-helper (cdr lst) (append checked (list (car lst)))))]
                           [else #f])))

(define multi-set? (lambda (lst)
                     (multi-set-helper lst '())))
					
;Program 2
(define ms-size (lambda (lst)
	(if 
		(null? lst)
		0
		(+ (cadr (car lst)) (ms-size (cdr lst)))
		)))

;Program 3
(define matrix-ref (lambda (m row col)
	(if 
		(> row 0)
		(matrix-ref (cdr m) (- row 1) col)
		(if 
			(> col 0)
			(matrix-ref (cons (cdr (car m)) (cdr m)) row (- col 1))
			(car (car m)))
		)))
	
;Program 4
(define matrix-helper 
	(lambda (lst lg)  
					(andmap (lambda (x) (equal? lg (length x))) lst)
					))
	
(define matrix? (lambda (lst)
	(if 
		(list? lst)
			(if 
				(null? lst)
				#f
				(if 
					(null? (car lst))
					#f
					(and (andmap (lambda (x) (list? x)) lst)(matrix-helper lst (length (car lst))))))
		#f
	)))
	
;Program 5
(define matrix-transpose (lambda (lst)
	(if 
		(equal? 0 (length (car lst)))
		'()
		(append (list (map car lst)) (matrix-transpose (map cdr lst)))
		)))

;Program 6
(define last-process (lambda (l result)
	(if (null? l) result (last-process (cdr l) (car l)))))

(define last (lambda (l) 
	(last-process l '())))

;Program 7
(define all-but-last (lambda (l)
	(if (null? (cdr l)) '() (append (list(car l)) (all-but-last (cdr l))) )))