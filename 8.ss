;Assignment 8

;Bo Peng

;Problem 1a
(define slist-map (lambda (proc slist)
	(if (null? slist)
		'()
		(cond
			[(not (list? slist)) (proc slist)]		
			
			[else (cons (slist-map proc (car slist))
									(slist-map proc (cdr slist)))]
			))))
			
;Problem 1b
(define slist-reverse (lambda (slist)
	(if (null? slist) 
		'()
		(reverse (map (lambda (x) (if (list? x) (slist-reverse x) x)) slist) ))))
		
;Problem 1c
(define slist-paren-count (lambda (slist)
	(if (list? slist)  (+ 2 (slist-paren-count-h slist)) 0)))
	
(define slist-paren-count-h (lambda (slist)
	(if 
				(null? slist) 
				0 
				(cond 
					[(list? (car slist)) (+ 2 (slist-paren-count-h (car slist))(slist-paren-count-h (cdr slist)))]
					[else (slist-paren-count-h (cdr slist))])))) 
					
;Problem 1d
(define slist-depth-h (lambda (slist)
	(if 
		(null? slist)
		(list 1)
		(cond 
			[(symbol? slist) 1]
			[(list? (car slist)) (cons (+ 1 (apply max (slist-depth-h (car slist))))
										(slist-depth-h (cdr slist)))]
			[(symbol? (car slist)) (cons 1
										(slist-depth-h (cdr slist)))]
			)
		)))

(define slist-depth (lambda (slist)
	(apply max (slist-depth-h slist) )))
	
;Problem 1e
(define slist-symbols-helper (lambda (slist depth target)
	(cond 
		[(equal? depth target) (cond 
									[(null? slist) '()]
									[(symbol? slist) (list slist)]
									[(list? (car slist)) (slist-symbols-helper (cdr slist) depth target)]
									[(symbol? (car slist)) (cons (car slist)(slist-symbols-helper (cdr slist) depth target))]
									)]
		[else (cond 
					[(null? slist) '()]
					[(symbol? slist) '()]
					[(list? (car slist)) (append 
												(slist-symbols-helper (car slist) (+ 1 depth) target)
												(slist-symbols-helper (cdr slist) depth target))]
					[(symbol? (car slist)) (slist-symbols-helper (cdr slist) depth target)]
					)]
		)))

(define slist-symbols-at-depth (lambda (slist d)
	(slist-symbols-helper slist 1 d)))
	
;Program 2		
(define subst-leftmost (lambda (new old slist equality-pred?)
	(if 
		(null? slist)
		'()
		(let loop ([current (car slist)][left (cdr slist)]) 
			(cond 
				[(null? left)(if 
								(equality-pred? current old) 
								(cons new left) 
								(cons current left))]
				[(or (null? current)(symbol? current))				 
					(if 
								(equality-pred? current old) 
								(cons new left) 
								(cons current(loop (car left) (cdr left)) ))
				]
				[else (cons (loop (car current) (cdr current)) left)])
				)
	))
)


		
		