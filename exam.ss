;Problem1
(define get-all-but-last (lambda (lst)
	(if (or (null? lst)(symbol? lst)) '() 
	(if (null? (cdr lst))
		'()
		(cons (car lst) (get-all-but-last (cdr lst)))))))
		
(define vector-list
	(lambda (vec)
		(let ([t (list->vector (vector->list vec))])
			(lambda (msg . args)
				(case msg ; Scheme's case is a similar to switch in some other languages.
					[(size) (vector-length t)]
					[(get) (vector-ref t (car args))]
					[(set) (vector-set! t (car args) (cadr args))]
					[(remove) (let 
								([temp (list->vector (vector->list t))])
									((set! t (list->vector(get-all-but-last (vector->list t))))
									(car (reverse temp))))]
					[else (errorf 'stack "illegal message to vetor-list object: ~a" msg)])))))