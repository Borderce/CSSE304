(define contains? (lambda (slist sym)
	(let in-list? ([slist slist])
		(cond 
			[(null? slist) #f]
			[(symbol? (car slist)) (or 
									(eq? (car slist)))
									(in-list? (cdr slist))]
			[else (or
					(in-list? (car slist))
					(in-list? (cdr slist)))]
					))))
					
(define count-occurrences (lambda (slist sym)
	(let count ([sl slist])
		(cond 
			[(null? sl) 0]
			[(symbol? (car sl))
			(+ (if (eq? sym (car sl)) 1 0) (count (cdr sl)))]
			[else (+ (count (car sl))(count (cdr sl)))]))))