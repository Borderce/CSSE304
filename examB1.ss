;Bo Peng

;Problem 1
(define group-by-two 
	(lambda (lst)
		(if (null? lst)
			'()
			(if (null? (cdr lst))
				(list lst)
				(cons (list (car lst) (cadr lst))  (group-by-two (cddr lst))) ))
		))
			 
;Problem 2
(define group-by-n (lambda (lst num)
	(if (null? lst) 
		'()
		(if (null? (group-by-n-helper-next lst num))
			(list lst)
			(cons (group-by-n-helper lst num '()) (group-by-n (group-by-n-helper-next lst num) num))))))
	
(define group-by-n-helper (lambda (lst num r)
	(if (or (equal? 0 num) (null? lst))
		r
		 (group-by-n-helper (cdr lst)(- num 1)(append r (list (car lst)))))))

(define group-by-n-helper-next (lambda (lst num)
	(if (or (equal? 0 num) (null? lst))
		lst
		(group-by-n-helper-next (cdr lst) (- num 1)))))

;Program 3
(define sorted? (lambda (lst)
	(andmap  <  lst (append (cdr lst ) (list (+ 1(car (reverse lst))))))))