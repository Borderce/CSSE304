;Bo Peng

;Program 1c

(define apply-continuation (lambda (k v) (k v)))

(define set-of-cps (lambda (s k)
	(cond 
		[(null? s) (apply-continuation k '())]
		[else 
			(set-of-cps 
				(cdr s) 
				(lambda (set-of-cps-cdr) (member?-cps 
											(car s) 
											(cdr s) 
											(lambda (member?-cps-result)
												(cond 
													[member?-cps-result (apply-continuation k set-of-cps-cdr)]
													[else (apply-continuation k (cons (car s) set-of-cps-cdr))])))))]
	)))

(define member?-cps (lambda (sym ls k)
	(cond 
		[(null? ls) (apply-continuation k #f)]
		[(equal? sym (car ls)) (apply-continuation k #t)]
		[else (member?-cps sym (cdr ls) k)])))
		
(define 1st-cps (lambda (s k)
	(apply-continuation k (car s))))

(define domain-cps (lambda (rel k)
	(map-cps 1st-cps rel (lambda (map-result) (set-of-cps map-result k)))))
	
(define make-cps (lambda (proc)
	(lambda (args k) (apply-continuation k (proc args)))))
	
