;Problem 1
(define make-c...r (lambda (ads)
	(make-c...r-helper (list->string(reverse (string->list ads))))))
(define make-c...r-helper (lambda (ads)
	(if (equal? 0 (string-length ads))
		(lambda (v) v)
		(cond 
			[(equal? '#\a (string-ref ads 0))
				(lambda (v) ((make-c...r-helper (substring ads 1 (string-length ads)))(car v)))]
			[(equal? '#\d (string-ref ads 0))
				(lambda (v) ((make-c...r-helper (substring ads 1 (string-length ads)))(cdr v)))]
			[else (display 'error)]))))
			
;Problem 2
(define permute (lambda (lst)
	(let loop ((current lst)(result '()))
		(if (null? current) 
			result
			(loop (cdr current) (append result (permute-part (car current)(get-rest (car current) lst))))))))
			
(define permute-part (lambda (first rest)
	(if (null? rest) (list (list first)) (map (lambda (v) (cons first v))(permute rest)))
	))
		
(define get-rest (lambda (current lst)
	(if (null? lst)
		'()
		(if (equal? (car lst) current)
			(get-rest current (cdr lst))
			(cons (car lst) (get-rest current (cdr lst)))))))
;Problem 3			
(define-datatype continuation continuation?
	[list-k]
	[id-k]
	[subst-k
		(new symbol?)
		(old symbol?)
		(slist list?)
		(k continuation?)]
	[subst-in-sym-exp-k 
		(new symbol?)
		(old symbol?)
		(se list?)
		(k continuation?)]
	[subst-k2
		(val (lambda (x) (or (symbol? x) (list x))))
		(k continuation?)])
	
(define apply-continuation (lambda (k v) (k v)))
	
(define apply-k
	(lambda (k val)
		(cases continuation k
			[id-k () val]
			[list-k ()
				(list val)]
			[subst-k (new old slist k2)
				(subst-cps new old slist (subst-k2 val k2))]
			[subst-k2 (val2 k2)
				(apply-k k2 (cons val2 val))]
			[subst-in-sym-exp-k (new old se k2)
				(subst-in-sym-exp-cps new old se k2)]
			)))	
	
	
(define subst-cps (lambda (new old slist k)
	(if (null? slist)
		(apply-k k '())
		(subst-in-sym-exp-cps new old (car slist) (subst-k new old (cdr slist) k)))))
		
(define subst-in-sym-exp-cps (lambda (new old se k)
	(if (symbol? se)
		(if (eqv? se old)
			(apply-k k new)
			(apply-k k se))
		(subst-cps new old se k))))
		
		
		
	