;Bo Peng & Max Morgan
;Assignment 11

;Problem 1a
(define-syntax my-let
	(syntax-rules () 
		[
			(_ func ((x v)...) e1 ...) 
			(letrec ((func (lambda (x ...) e1 ...))) (func v ...))
		]
		[
			(_ ((x v) ...) e1 ...)
			((lambda (x ...) e1 ...) v ...)
		]))
		
;Problem 1b
(define-syntax my-or
	(syntax-rules ()
		[(_) #f ]
		[(_ e1) e1]
		[(_ e1 e2 ...) (let ((temp e1))(if temp temp (my-or e2 ...)) )]))

;Problem 1c
(define-syntax +=
	(syntax-rules ()
		[(_ a b) (let 
					((temp a)) 
					(begin 
						(set! a (+ a b))
						a))]
		))
		
;Problem 1d
(define-syntax return-first
	(syntax-rules ()
		[(_ v) v]
		[(_ v1 v2 ...) (let ((temp v1)) (begin v2 ...) temp)]))

;Problem 2
(define-datatype bintree bintree?
	(leaf-node
		(num integer?))
	(interior-node
		(key symbol?)
		(left-tree bintree?)
		(right-tree bintree?)))
		
(define bintree-to-list (lambda (T) 
	(cases bintree T
		[leaf-node (num) (list 'leaf-node num)]
		[interior-node (key left-tree right-tree) (list 
													'interior-node 
													key 
													(bintree-to-list left-tree) 
													(bintree-to-list right-tree)
													)])))

;Problem 3
(define max-interior-helper (lambda (T)
	(cases bintree T
		[leaf-node (num) num]
		[interior-node (key left-tree right-tree) 
			(cases bintree left-tree
				[leaf-node (numl) 
					(cases bintree right-tree
						[leaf-node (numr) (list (+ numl numr) key (+ numl numr))]
						[interior-node (key-r left-tree-r right-tree-r)
							(let* 
								((templeft numl)
								(tempright (max-interior-helper right-tree))
								(tempsum (+ numl (car tempright))))
								(if (< tempsum (caddr tempright))
									(list tempsum (cadr tempright) (caddr tempright))
									(list tempsum key tempsum)))])]
				[interior-node (key-l left-tree-l right-tree-l) 
					(cases bintree right-tree
						[leaf-node (numr) 
							(let* 
								((templeft (max-interior-helper left-tree)) 
								(tempright numr)
								(tempsum (+ (car templeft) tempright))) 
								(if (> tempsum (caddr templeft))
									(list tempsum key tempsum)
									(list tempsum (cadr templeft) (caddr templeft))))]
						[interior-node (key-r left-tree-r right-tree-r)
							(let* 
								((templeft (max-interior-helper left-tree)) 
								(tempright (max-interior-helper right-tree))
								(tempsum (+ (car templeft) (car tempright)))) 
								(if (eqv? tempsum (max tempsum (car templeft) (car tempright)))
									(list tempsum key tempsum)
									(if (< (caddr templeft) (caddr tempright)) 
										(list tempsum (cadr tempright) (caddr tempright))
										(list tempsum (cadr templeft) (caddr templeft)))))])])
		])))

(define max-interior (lambda (T)
	(cadr (max-interior-helper T) )))

;Problem 4
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define all-sym? (lambda (lst)
	(if (null? lst)
		#t
		(if (symbol? (car lst))
			(all-sym? (cdr lst))
			#f))))

;(define-datatype expression expression?
;	(var-exp
;		(sym symbol?))
;	(lit-exp
;		(num number?))
;	(app-exp
;		(rator expression?)
;		(rand expression?))
;	(lambda-exp
;		(id symbol?)
;		(body expression?))
;	(if-exp
;		(con expression?)
;		(true expression?)
;		(false expression?))
;)			
			

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
	 [(vector? datum) (vec-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? 'lambda (1st datum)) (lambda-exp datum)]
	   [(eqv? 'if (1st datum)) (if-exp datum)]
	   [(eqv? 'let (1st datum)) (let-exp datum 'let)]
	   [(eqv? 'letrec (1st datum)) (let-exp datum 'letrec)]
	   [(eqv? 'let* (1st datum)) (let-exp datum 'let*)]
	   [(eqv? 'set! (1st datum)) (set-exp datum)]
	   [(not (list? datum)) (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
       [else (app-exp (parse-exp (1st datum))
		      (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define vec-exp (lambda (datum)
	(list 'vec-exp datum)))
	 
(define var-exp (lambda (datum)
	(list 'var-exp datum)))

(define lit-exp (lambda (datum)
	(list 'lit-exp datum)))
	
(define app-exp (lambda (func var)
	(list 'app-exp (list func var))))
	
(define set-exp (lambda (datum)
	(cond 
		[(not (eqv? 3 (length datum))) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
		[else (list 'set!-exp (parse-exp (2nd datum))(parse-exp (3rd datum)))])))

(define let-exp (lambda (datum type)
	(cond 
		[(not (>= (length datum) 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
		[(not (list? (2nd datum))) (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" datum)]
		[(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" datum)]
		[(not (andmap (lambda (lst) (eqv? 2 (length lst))) (2nd datum))) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" datum)]
		[(not (andmap (lambda (lst) (symbol? (1st lst))) (2nd datum))) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" datum)]
		[else (cond 
					[(eqv? 'let type)(list 'let-exp (parse-exp-list (map 1st (2nd datum)))(parse-exp-list (map 2nd (2nd datum))) (parse-exp-list (cddr datum)))]
					[(eqv? 'let* type)(list 'let*-exp (parse-exp-list (map 1st (2nd datum)))(parse-exp-list (map 2nd (2nd datum))) (parse-exp-list (cddr datum)))]
					[(eqv? 'letrec type)(list 'letrec-exp (parse-exp-list (map 1st (2nd datum)))(parse-exp-list (map 2nd (2nd datum))) (parse-exp-list (cddr datum)))]
					[else (eopl:error 'parse-exp "invalid let expression ~s" datum)])])))	

(define parse-exp-list (lambda (lst)
	(if 
		(null? lst)
		'()
		(append 
			(list(parse-exp (1st lst)))
			(parse-exp-list (cdr lst))))))
		
(define if-exp (lambda (datum)
	(cond
		[(not (eqv? 4 (length datum))) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
		[else (list
					'if-exp
					(parse-exp (2nd datum))
					(parse-exp (3rd datum))
					(parse-exp (4th datum)))])))
	
	
(define lambda-exp (lambda (datum)
	(cond 
		[(not (>= (length datum) 3)) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
		[(symbol? (2nd datum)) (cond 
									[(not (>= (length datum) 3)) (eopl:error 'parse-exp "lambda-expression missing body")]
									[else (list 'lambda-exp (parse-exp (2nd datum)) (parse-exp-list (cddr datum)))])]
		[(not (all-sym? (2nd datum))) (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" datum)]
		[(list? (2nd datum)) (list 'lambda-exp (parse-exp-list (2nd datum)) (parse-exp-list (cddr datum)))]
		)))
	
(define unparse-exp (lambda (datum)
	(cond
		[(eqv? (1st datum) 'lambda-exp)
			(cond 
				[(null? (2nd datum)) (append (list 'lambda) (list '()) (unparse-exp-list (3rd datum)))]
				[(eqv? 'var-exp (1st(2nd datum)))(append (list 'lambda) (list (2nd (2nd datum))) (unparse-exp-list (3rd datum)))]
				[else (append (list 'lambda) (list(unparse-exp-list (2nd datum))) (unparse-exp-list (3rd datum)))])]
		[(eqv? (1st datum) 'var-exp)(2nd datum)]
		[(eqv? (1st datum) 'lit-exp)(2nd datum)]
		[(eqv? (1st datum) 'vec-exp)(2nd datum)]
		[(eqv? (1st datum) 'if-exp)(append (list 'if) (list(unparse-exp (2nd datum))) (list(unparse-exp (3rd datum))) (list(unparse-exp (4th datum))))]
		[(eqv? (1st datum) 'let-exp)(append (list 'let) (list(un-parse-let (2nd datum)(3rd datum)))(unparse-exp-list (4th datum)))]
		[(eqv? (1st datum) 'let*-exp)(append (list 'let*)(list(un-parse-let (2nd datum)(3rd datum)))(unparse-exp-list (4th datum)))]
		[(eqv? (1st datum) 'letrec-exp)(append (list 'letrec)(list(un-parse-let (2nd datum)(3rd datum)))(unparse-exp-list (4th datum)))]
		[(eqv? (1st datum) 'set!-exp)(append (list 'set) (unparse-exp (2nd datum)) (unparse-exp (3rd datum)))]
		[(eqv? (1st datum) 'app-exp)(append (list(unparse-exp (1st (2nd datum))))(unparse-exp-list (2nd (2nd datum))))]
		[else (eopl:error 'unparse-exp "invalid unparse expression ~s" datum)])))

(define un-parse-let (lambda (var-lst exp-lst)
	(if 
		(or (null? var-lst) (null? exp-lst))
		'()
		(append
			(list(list 
				(unparse-exp (car var-lst)) 
				(unparse-exp (car exp-lst))))
			(un-parse-let
				(cdr var-lst)
				(cdr exp-lst))))))
		
(define unparse-exp-list (lambda (lst)
	(if 
		(null? lst)
		'()
		(append
			(list (unparse-exp (1st lst)))
			(unparse-exp-list (cdr lst))))))