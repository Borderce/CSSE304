;Problem 1
(define apply-continuation (lambda (k v) (k v)))

(define member?-cps (lambda (sym ls k)
	(cond 
		[(null? ls) (apply-continuation k #f)]
		[(equal? sym (car ls)) (apply-continuation k #t)]
		[else (member?-cps sym (cdr ls) k)])))

			
(define set?-cps
	(lambda (ls k)
		(cond [(null? ls) (apply-continuation k '#t)]
		[(not (pair? ls)) (apply-continuation k '#f)]
		[else (set?-cps (cdr ls)
					(lambda (cdr-result)
						(member?-cps (car ls) (cdr ls)
							(lambda (is-member?)
								(apply-continuation k
									(if is-member?
										'#f
										cdr-result))))))])))

(define make-cps
	(lambda (proc)
		(lambda (arg k)
			(apply-continuation k (proc arg)))))
	  
(define 1st-cps
	(lambda (ls k)
		(apply-continuation k (car ls))))			
	
(define set-of-cps (lambda (s k)
		(cond [(null? s) (apply-continuation k '())]
			[else
				(set-of-cps (cdr s)
					(lambda (cdr-result)
						(member?-cps (car s) (cdr s)
							(lambda (is-member?)
								(apply-continuation k
									(if is-member?
										cdr-result
										(cons (car s) cdr-result)))))))])))
(define map-cps
	(lambda (proc-cps ls k)
		(cond [(null? ls) (apply-continuation k '())]
			[else 
				(map-cps proc-cps (cdr ls)
					(lambda (cdr-result)
						(proc-cps (car ls)
							(lambda (proc-result)
								(apply-continuation k
									(cons proc-result cdr-result))))))])))



									
(define domain-cps (lambda (rel k) (map-cps 1st-cps rel (lambda (map-result)
		(set-of-cps map-result k)) )))		
		
		
(define andmap-cps
	(lambda (pred-cps ls k)
		(cond 
			[(null? ls) (apply-continuation k '#t)]
			[else (pred-cps (car ls)(lambda (proc-result)
										(if proc-result
							(andmap-cps pred-cps (cdr ls)
								(lambda (cdr-result)
									(apply-continuation k cdr-result)))
							(apply-continuation k '#f))))])))
	
	
(define cps-snlist-recur
	(lambda (base-value item-proc-cps list-proc-cps)
		(letrec
			([helper (lambda (ls k)
				(if (null? ls)
					(apply-continuation k base-value)
					(let ([a (car ls)])
									(if (or (pair? a) (null? a))
									(helper a
										(lambda (car-result)
											(helper 
												(cdr ls)
												(lambda (cdr-result)
													(list-proc-cps car-result cdr-result k)))))
									(helper (cdr ls)
										(lambda (cdr-result)
											(item-proc-cps a cdr-result k)))))))])
			helper)))
(define append-cps
	(lambda (ls1 ls2 k)
		(cond [(null? ls2) (apply-continuation k ls1)]
			[else 
				(append-cps 
					(reverse (cons (car ls2) (reverse ls1))) 
					(cdr ls2) 
					k)])))
					
(define sn-list-reverse-cps
	(cps-snlist-recur '()
		(lambda (x y k) (append-cps y (list x) k))
		(lambda (x y k) (append-cps y (list x) k))))
(define +-cps
	(lambda (a b k)
		(apply-continuation k (+ a b))))
(define sn-list-occur-cps
	(lambda (s snlist k)
		((cps-snlist-recur 0
			(lambda (x y k)
				(if (eq? s x)
					(+-cps 1 y k)
					(apply-continuation k y)))
			+-cps) snlist k)))
(define max-cps
	(lambda (a b k)
		(apply-continuation k (max a b))))
(define sn-list-depth-cps
	(cps-snlist-recur 1
		(lambda (x y k) (apply-continuation k y))
		(lambda (x y k) (max-cps (+ 1 x) y k))))	
	
	
(define memoize 
	(lambda (f hash equiv?)	
		(let ((table (make-hashtable hash equiv?)))
		(lambda x
			(let ((return (hashtable-ref table x #f)))
			(if return 
				return
				(let ((obj (apply f x)))
					(hashtable-set! table x obj) obj))))))) 
			
			
			
(define-syntax with-values
	(syntax-rules ()
		[(_ expr consumer)
			(call-with-values
				(lambda () expr)
					consumer)]))
(define-syntax mv-let
	(syntax-rules ()
		((_ ((x ...) e0) e1 e2 ...)
			(with-values e0
				(lambda (x ...) e1 e2 ...)))))
				
(define subst-leftmost
	(lambda (new old slist proc)
		(with-values
			(let helper ([ls slist] [current (list)])
				(cond [(null? ls) (values (append current ls) #f)]
					[(symbol? (car ls))
						(if (proc (car ls) old)
							(values 
								(append (append current (list new)) (cdr ls))
								#t)
							(helper (cdr ls) (append current (list (car ls)))))]
					[else
						(mv-let ((inner-ls changed?) (helper (car ls) (list)))
							(let ([outer-ls (append current (list inner-ls))])
								(if changed?
									(values (append outer-ls (cdr ls)) #t)
									(helper (cdr ls) outer-ls))))]))
			(lambda (a b) a))))
	