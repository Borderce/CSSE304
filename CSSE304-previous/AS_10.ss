(define make-stack
	(lambda ()
		(let ([stack '()])
			(lambda (msg  . args )
    		(case msg
      		[(empty?) (null? stack)]
      		[(push)   (set! stack (cons (car args) stack))]
      		[(pop)    (let ([top (car stack)])
                   		(set! stack (cdr stack))
                   	top)]
      		[else (errorf 'stack "illegal message to stack object: ~a" msg)]))))) 
 
(define make-slist-leaf-iterator
	(lambda (ls)
		(let ([stack (make-stack)])
			(stack 'push ls)
			(letrec (
			
				[iter-helper 
					(lambda ()
          	(set! stack stack)
            (if (stack 'empty?)
      				#f
             	(let ([top-ele (stack 'pop)])
                (cond
                 	[(null? top-ele) (iter-helper)]
                 	[(list? top-ele) (sub-iter top-ele)]
                  [(symbol? top-ele) top-ele]))))]
									
				[sub-iter 
					(lambda (ls)
          	(set! stack stack)
            	(cond
              	[(null? ls) (iter-helper)]
               	[(null? (car ls)) (sub-iter (cdr ls))]
                [(symbol? (car ls)) (begin (stack 'push (cdr ls))
                                  				 (car ls))]
        				[(list? (car ls)) (begin (stack 'push (cdr ls))
                                  			 (sub-iter (car ls)))]))])
  iter-helper))))

(define remove-element-from-set 
	(lambda (element set)
  	(cond 
			[(null? set) set]
			[(equal? (car set) element) (cdr set)]
			[else (cons (car set) (remove-element-from-set element (cdr set)))])))

(define remove-elements-from-set 
	(lambda (elements set)
  	(cond 
			[(or (null? elements) (null? set)) set]
			[else (remove-elements-from-set (cdr elements)
																			(remove-element-from-set (car elements) set))])))

(define element-of? 
	(lambda (element set)
  	(cond 
			[(null? set) #f]
			[(equal? element (car set)) #t]
			[else (element-of? element (cdr set))])))

(define union-set 
	(lambda (set1 set2)
  	(cond 
			[(null? set1) set2]
			[(element-of? (car set1) set2) (union-set (cdr set1) set2)]
			[else (cons (car set1) (union-set (cdr set1) set2))])))

(define let-vars 
	(lambda (let-exp)
  	(map car (cadr let-exp))))

(define let-exps 
	(lambda (let-exp)
  	(map cadr (cadr let-exp))))
		
(define ispred?
	(lambda (pred)
		(member? pred (list 'lambda 'if 'let 'let* 'letrac 'set!))))

(define free-vars 
	(lambda (exp)
  	(cond 
			[(null? exp) '()]
      [(number? exp) 'num '()]
			[(symbol? exp) (list exp)]
			
     	[(equal? (car exp) 'let)
				(cond 
				 	[(null? (cadr exp)) (free-vars (caddr exp))]
          [else (union-set (free-vars (let-exps exp))
                  				 (remove-elements-from-set (let-vars exp)
                       															 (free-vars (caddr exp))))])]
      [(equal? (car exp) 'let*)
         (cond 
				 	[(null? (cadr exp)) (free-vars (caddr exp))]
          [else (union-set (free-vars (cadr (caadr exp)))
                      		 (remove-element-from-set (caaadr exp)
                       															(free-vars (list 'let* (cdr (cadr exp)) (caddr exp)))))])]
																										
      [(equal? (car exp) 'lambda) (remove-elements-from-set (cadr exp) 
																														(free-vars (caddr exp)))]
			
			[(equal? (car exp) 'if) (union-set (free-vars (cadr exp))
																				 (union-set (free-vars (caddr exp)) 
																				 						(free-vars (cadddr exp))))]
			[else (union-set (free-vars (car exp))
			 								 (free-vars (cdr exp)))])))
	
	
(define set
	(lambda (ls)
		(letrec ([helper 
			(lambda (ls)
    		(cond
       		[(null? ls) '()]
         	[(memq (car ls) (cdr ls)) (append (helper (cdr ls)))]
  				[else (append (list (car ls)) (helper (cdr ls)))]))])
    (helper ls))))
	 
(define bound-vars
	(lambda (exp)
	 	(letrec (
		
			[helper 
				(lambda (exp)
					(cond
						[(null? exp) '()]
	  				[(symbol? exp) '()]
	  				[(eq? 'lambda (car exp)) (vars-helper (cadr exp) (caddr exp))]
	  				[else (append (helper (car exp)) (helper (cdr exp)))]))]
						
	     [vars-helper 
			 	(lambda (var arg)
	      	(cond
	      		[(null? arg) '()]
	      		[(symbol? arg) (if (memq arg var)
	                    				 (list arg)
	                     				 '())]
	   				[(eq? 'lambda (car arg)) (vars-helper (append var (cadr arg)) (caddr arg))]
	          [(memq (car arg) var) (append (list(car arg)) (vars-helper var (cdr arg)))]
	        	[else (append (vars-helper var (cadr arg)))]))])
						
	(set (helper exp)))))	 
	  
(define occurs-free? 
	(lambda (var exp)
		(element-of? var (free-vars exp))))
;		(or (ispred? var) (element-of? var (free-vars exp)))))
	 
(define occurs-bound? 
	(lambda (var exp)
		(or (not (occurs-free? var exp)) (element-of? var (bound-vars exp)))))
		
(define lexical-address
  (lambda (expr)
    (letrec (
			(list-of 
         (lambda (pred?)
           (lambda (val)
             (or (null? val)
               	 (and (pair? val)
                      (pred? (car val))
                 		  ((list-of pred?) (cdr val)))))))
       (expr? 
         (lambda (expr)
           (cond
             ((symbol? expr) #t)
             ((list? expr)
                (cond
                  ((eqv? (car expr) 'if) (and (= 4 (length expr))
                                							(expr? (cadr expr))
                       											 	(expr? (caddr expr))
                       											 (expr? (cadddr expr))))
																						 
                  ((eqv? (car expr) 'lambda) (and (= 3 (length expr))
                       											 ((list-of symbol?) (cadr expr))
                       											 (expr? (caddr expr))))
																						 
                  (else (and (<= 1 (length expr))
                      			 ((list-of expr?) expr)))))
														 
             (else #f))))
       
			 (position
         (lambda (symbol d p names)
           (cond
             ((null? names) '())
             ((eqv? symbol (car names)) (list ': d p))
             (else (position symbol d (+ 1 p) (cdr names))))))
						 
       (depth
         (lambda (symbol d stack)
           (if (null? stack)
             (list ': 'free symbol)
             (let ((result (position symbol d 0 (car stack))))
               (if (null? result)
                 (depth symbol (+ d 1) (cdr stack))
                 result)))))
								 
       (la
         (lambda (expr stack)
           (cond
             ((symbol? expr) (depth expr 0 stack))	
             ((eqv? (car expr) 'if) (cons 'if (map (lambda (x) (la x stack)) (cdr expr))))
             ((eqv? (car expr) 'lambda) (list 'lambda
                  											(cadr expr)
                  											(la (caddr expr) (cons (cadr expr) stack))))
             (else (map (lambda (x) (la x stack)) expr))))))

     (if (expr? expr) (la expr '()) #f))))


(define un-lexical-address
  (lambda (expr)
    (letrec(
			(list-of 
         (lambda (pred?)
           (lambda (val)
             (or (null? val)
               (and (pair? val)
                 		(pred? (car val))
                 	 	((list-of pred?) (cdr val)))))))
       (expr? 
         (lambda (expr stack)
           (if (list? expr)
             (cond
               ((eqv? (cadr expr) 'free) #t) 
               
							 ((eqv? (car expr) ':) (and (= 3 (length expr))
                    											(number? (cadr expr))
                    											(< (cadr expr) (length stack))
                    											(>= (cadr expr) 0)
                    											(number? (caddr expr))
                    											(< (caddr expr) (length (list-ref stack (cadr expr))))
                    											(>= (caddr expr) 0)))
																					
               ((eqv? (car expr) 'if) (and (= 4 (length expr))
                   	 											 (expr? (cadr expr) stack)
                   										 		 (expr? (caddr expr) stack)
                    											 (expr? (cadddr expr) stack)))
																					 
               ((eqv? (car expr) 'lambda) (and (= 3 (length expr))
                    													 ((list-of symbol?) (cadr expr))
                    													 (expr? (caddr expr) (cons (cadr expr) stack))))
																							 
               (else (and (<= 1 (length expr))
                   				((list-of (lambda (x) (expr? x stack))) expr))))
           #f)))
					 
       (ula
         (lambda (expr stack)
           (cond
           		((and (eqv? (car expr) ':) (eqv? (cadr expr) 'free)) (car (cdr (cdr expr))))
							
             	((eqv? (car expr) ':) (list-ref (list-ref stack (cadr expr)) (caddr expr)))
							
             	((eqv? (car expr) 'if) (cons 'if (map (lambda (x) (ula x stack)) (cdr expr))))
							
            	((eqv? (car expr) 'lambda) (list 'lambda
                 	 														 (cadr expr)
                  														 (ula (caddr expr) (cons (cadr expr) stack))))
             
             (else (map (lambda (x) (ula x stack)) expr))))))
						 
      (if (expr? expr '()) 
				(ula expr '()) 
				#f))))