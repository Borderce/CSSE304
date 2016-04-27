; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
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
			

(define parse-exp         
  (lambda (datum)
	(cond
     [(symbol? datum) (var-exp-p datum)]
     [(pair? datum)
      (cond
	   [(eqv? 'repeat (1st datum)) (repeat-exp-p datum)]
       [(eqv? 'lambda (1st datum)) (lambda-exp-p datum)]
	   [(eqv? 'if (1st datum)) (if-exp-p datum)]
	   [(eqv? 'let (1st datum)) (let-exp-p datum 'let)]
	   [(eqv? 'letrec (1st datum)) (let-exp-p datum 'letrec)]
	   [(eqv? 'let* (1st datum)) (let-exp-p datum 'let*)]
	   [(eqv? 'set! (1st datum)) (set-exp-p datum)]
	   [(eqv? 'quote (1st datum)) (quote-exp-p (cadr datum))]
	   [(not (list? datum)) (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
       [else (app-exp-p (parse-exp (1st datum))
		      (map parse-exp (cdr datum)))])]
	 [(lit? datum) (lit-exp-p datum)]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))
    )

(define lit? (lambda (datum)
	(ormap 
       (lambda (pred) (pred datum))
		(list number? vector? boolean? symbol? string? pair? null?))))	
	
(define quote-exp-p (lambda (datum)
	(list 'quote-exp datum)))
	
(define vec-exp-p (lambda (datum)
	(list 'vec-exp datum)))
	 
(define var-exp-p (lambda (datum)
	(list 'var-exp datum)))

(define lit-exp-p (lambda (datum)
	(list 'lit-exp datum)))
	
(define app-exp-p (lambda (func var)
	(list 'app-exp func var)))
	
(define set-exp-p (lambda (datum)
	(cond 
		[(not (eqv? 3 (length datum))) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
		[else (list 'set!-exp (parse-exp (2nd datum))(parse-exp (3rd datum)))])))

(define let-exp-p (lambda (datum type)
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

(define repeat-exp-p (lambda (datum)
	(cond 
		[(not (>= (length datum) 3)) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
		[else  (cond [(or (list? (2nd datum))(symbol? (2nd datum)))(list 'repeat-exp-l (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
						[else (list 'repeat-exp 
						(let loop 
							([num (2nd datum)]) 
							(if (eqv? 0 num)
										'()
										(cons (parse-exp (3rd datum)) (loop (- num 1))))))])])))
					
(define parse-exp-list (lambda (lst)
	(if 
		(null? lst)
		'()
		(append 
			(list(parse-exp (1st lst)))
			(parse-exp-list (cdr lst))))))
		
(define if-exp-p (lambda (datum)
	(cond
		[(not (eqv? 4 (length datum))) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
		[else (list
					'if-exp
					(parse-exp (2nd datum))
					(parse-exp (3rd datum))
					(parse-exp (4th datum)))])))
	
	
(define lambda-exp-p (lambda (datum)
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





