;Problem 1(a)
(load "chez-init.ss")

(define-syntax my-let
	(syntax-rules ()
		((_ ((name1 val1)  ...) body ...)
			((lambda (name1 ...) body ...) val1 ...))
		((_ func ((name1 val1)  ...) body ...)
			(letrec  ((func (lambda (name1 ...) body ...))) (func val1 ...) ))))
	
;Problem 1(b)	
(define-syntax my-or 
	(syntax-rules ()
		((_) #f)
		((_ first) first)
		((_ first second ...)
			(let ((temp first)) (if temp temp (my-or second ...)))
			)))	 
	 
;Problem 1(c)
(define-syntax +=
	(syntax-rules ()
		((_ ele content) (begin (set! ele (+ ele content)) ele ))))
		   
;Problem 1(d)
(define-syntax return-first
	(syntax-rules ()
		((_ first) first )
		((_ first second ...)  (let ((temp first))  (begin  second ...) temp)    )  
		))

;Problem 2		
(define-datatype bintree bintree?
	(leaf-node
		(num integer?))
	(interior-node
		(key symbol?)
		(left-tree bintree?)
		(right-tree bintree?)))
		
(define inorder
	(lambda (tree)
		(cases bintree tree 
			[leaf-node (datum) '()]
			[interior-node (key left right)
				(append (inorder left)
						(list key)
						(inorder right))])))
			
(define bintree-to-list 
	(lambda (tree)
		(cases bintree tree 
			[leaf-node (datum) (list 'leaf-node datum)]
			[interior-node (key left right)
				(append (list 'interior-node key)
						(list (bintree-to-list left))
						(list (bintree-to-list right)))])
		))
;Problem 3
(define interior-node? (lambda (node)
	(eqv? (car node) 'interior-node)))
	
(define leaf-node? (lambda (node)
	(eqv? (car node) 'leaf-node)))

(define max-iterator-helper 
	(lambda (tree)
		(cases bintree tree
			[leaf-node (datum) (list 'leaf-node datum)]
			[interior-node (key left right)
				
					(let ((lp (max-iterator-helper left))
						(rp (max-iterator-helper right))) (cond 
						[(and (interior-node? left) (interior-node? right)) 							
																		(cond 
																			[(and (< (cadr lp) 0) (< (cadr rp) 0)) 
																				(if (< (cadr lp) (cadr rp)) rp lp)]
																			[(< (cadr lp) 0)  rp]
																			[(< (cadr rp) 0)  lp]
																			[else (list key (+ (cadr lp) (cadr rp)))] )]
						[(interior-node? left) (if (< (cadr right) 0) lp (list key (+ (cadr lp) (cadr right)) ))]
						[(interior-node? right) (if (< (cadr left) 0) rp (list key (+ (cadr rp) (cadr left))) ) ]
						[else (list key (cond 
											[(and (list? left) (list? right))  (+ (cadr left) (cadr right))]
											[(list? left) (+ (cadr left) right)]
											[(list? right) (+ left (cadr right))]
											[else (+ left right)]))]
					))
					
					])))
					
(define max-interior (lambda (tree)
	(car (max-iterator-helper tree))))
	
;Problem 4(a)
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
		[(equal? 'lambda (1st datum)) (lambda-exp datum)]
		[(equal? 'if (1st datum)) (if-exp datum)]
		[(equal? 'let (1st datum)) (let-exp datum)]
		[(equal? 'let* (1st datum)) (let-exp datum)]
		[(equal? 'letrec (1st datum)) (let-exp datum)]
		[(equal? 'set! (1st datum)) (set-exp datum)]
		[(not (list? datum)) (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
        [else (app-exp (parse-exp (1st datum))
		      (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
	
(define set-exp (lambda (datum)
	(cond	
	[(not (equal? (length datum) 3)) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
	[else 'x])))
	
(define sym-all? (lambda (lst)
	(if (null? lst) #t (if (symbol? lst) #t (if (symbol? (car lst)) (sym-all? (cdr lst)) #f)))))
	
(define list-all? (lambda (lst)
	(if (null? lst) #t (if (list? (car lst)) (list-all? (cdr lst)) #f))))
	
(define app-exp (lambda (datum) (display 'xxx)))
(define var-exp (lambda (datum) (list 'var-exp datum)))
(define lit-exp (lambda (datum) (list 'lit-exp datum)))

(define lst-exp (lambda (datum)
	(if (null? datum) 
		(parse-exp datum) 
		(if (list? (cdr datum)) 
			(lst-exp (cdr datum)) 
			(eopl:error 'parse-exp "expression ~s is not a proper list" datum)))))

(define if-exp (lambda (datum)
	(cond 
		[(equal? (length datum) 4) (begin (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
		[else (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)])))

(define lambda-exp (lambda (datum)
	(cond
		[(not(equal? (length datum) 3)) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
		[(not(sym-all?(2nd datum))) (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" datum)]
		[else (parse-exp (3rd datum))])))
		
(define all-2? (lambda (lst)
	(if (null? lst) #t (if (equal? 2 (length (car lst))) (all-2? (cdr lst)) #f))))

(define all-2 (lambda (lst)
	(if (null? lst) '() (append (list (2nd(1st lst))) (all-2 (cdr lst))))))
(define all-1 (lambda (lst)
	(if (null? lst) '() (append (list (1st(1st lst))) (all-1 (cdr lst))))))	
(define let-exp (lambda (datum)
	(cond 
		[(not (equal? (length datum) 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
		[(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" datum)]
		[(not (list-all? (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" datum)]
		[(not (all-2? (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" datum)]
		[(not (sym-all? (all-1 (2nd datum)))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" datum)]
		[else (begin (parse-exp-lst (all-2 (2nd datum)) )(parse-exp (3rd datum)) )]
		)))		

(define parse-exp-lst (lambda (lst)
	(if (null? lst) 'x (begin (parse-exp (1st lst)) (parse-exp-lst (cdr lst)))  )))

		
(define unparse-exp (lambda (expre)
	(cases expression expre
		[var-exp (id) id]
		[lambda-exp (id body)
				(list 'lambda (list id)
				(unparse-exp body))]
		[app-exp (rator rand)
			(list (unparse-exp rator)
				(unparse-exp rand)
			)])))		
			
			
(define (subst-left-cps new old slist changed unchanged)
	(let loop(
			[slist slist]
			[changed changed]
			[unchanged unchanged])
		(cond 
			[(null? slist) (apply-continuation unchanged)]
			[(symbol? (car slist)) (if 
									(eq? (car slist) old)
									(apply-continuation changed (cons new (cdr slist)))
									(loop (cdr slist)
										(lambda (changed-cdr)
											(apply-continuation changed (cons (car slist) changed-cdr)))
											unchanged))]
			[else ; car is an slist
				(loop (car slist)
					(lambda (changed-car)
						(apply-continuation changed (cons changed-car (cdr slist))))
					(lambda ()
						(loop (cdr slist)
							(lambda (changed-cdr)
								(apply-continuation changed (cons (car slist) changed-cdr)))
								unchanged)))]
											)))
		