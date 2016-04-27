;Problem 1
(define make-stack
	(lambda ()
		(let ([stk '()])
			(lambda (msg . args )
				(case msg ; Scheme's case is a similar to switch in some other languages.
					[(empty?) (null? stk)]
					[(push) (set! stk (cons (car args) stk))]
					[(pop) (let ([top (car stk)])
						(set! stk (cdr stk))
						top)]
					[else (errorf 'stack "illegal message to stack object: ~a" msg)])))))
		


(define make-slist-leaf-iterator
	(lambda (slist)
		(let ([stack (make-stack)]) 
			(begin
				(stack 'push slist)
				(lambda () 
					(cond 
						[(stack 'empty?) #f]
						[else (letrec 
							([helper 
									(begin (if (stack 'empty?) #f (lambda () (let ([val (stack 'pop)])
									(if (null? val) (helper) (if (null? (cdr val)) 
										(cond 
											[(null? (car val)) (if (stack 'empty?) #f (helper))]
											[(list? (car val)) (begin (stack 'push (car val)) (helper)) ]
											[else (car val)]) 
										(begin 
											(stack 'push (cdr val)) 
											(cond 
												[(null? (car val)) (helper)]
												[(list? (car val)) (begin (stack 'push (car val)) (helper) )]
												[else (car val)]))))
									))) )
									]) (helper)) ]))))))
									
;Problem 2
(define contain? (lambda (e l)
	(if (null? l) 
		#f 
		(if (symbol? l) (if (eqv? e (car (list l))) #t (or #f (contain? e (cdr (list l))))) (if (eqv? e (car l)) #t (or #f (contain? e (cdr l))))))))
	
(define find-duplicate (lambda (l1 l2)
	(if (symbol? l2) 
		(if (contain? l2 l1) (list l2) '()) 
		(if 
			(null? l2) 
			'() 
			(if 
				(contain? (car l2) l1) 
				(append (list (car l2)) (find-duplicate l1 (cdr l2))) 
				(find-duplicate l1 (cdr l2)))))))
	

(define let*-check (lambda (slist)
	(if (eqv? (length slist) 1) 
		'() 
		(if (eqv?(car(car slist)) (cadr(cadr slist))) 
			(append (list (car(car slist))) (let*-check (cdr slist))) 
			(let*-check (cdr slist)))))) 
	
	
(define bound-vars-helper (lambda (slist)
	(if (null? slist) 
		'() 
		(cond [(list? slist) 
					(cond 
						[(equal? 'lambda (car slist)) 
								(let ([cell (cadr slist)])
									(append (find-duplicate cell (caddr slist))(bound-vars-helper (caddr slist))))]
						[(equal? 'let* (car slist)) (append 
														(find-duplicate (every-first-from-list (cadr slist)) (flat(caddr slist))) 
														(let*-check (cadr slist)) 
														(bound-vars (caddr slist)))]
						[(equal? 'let (car slist)) (append (find-duplicate (every-first-from-list (cadr slist)) (flat(caddr slist))) (bound-vars (caddr slist)))] 
						[(list? (car slist)) (append (bound-vars-helper (car slist)) (bound-vars-helper (cdr slist))) ]
						[else (bound-vars-helper (cdr slist))]
						)]
			  [(symbol? slist) '()]
			  [else '()]))))

(define no-duplicate (lambda (slist result)
	(if (null? slist) 
		result 
		(if
			(contain? (car slist) result) 
			(no-duplicate (cdr slist) result) 
			(no-duplicate (cdr slist) (append result (list(car slist))))))))
	
(define bound-vars (lambda (slist)
	(let ([result (bound-vars-helper slist)])(no-duplicate result '()))))
	
(define  (flat slist)
    (let flat ([slist slist])
      (cond 
		[(null? slist) '()]
		[(symbol? slist) (list slist)]
	    [(symbol? (car slist)) (cons (car slist)
					 (flat (cdr slist)))]
	    [else (append (flat (car slist)) (flat (cdr slist)))])))
	
(define intersect (lambda (l1 l2)
	(if 
		(null? l2) 
		'() 
		(if (list? l2)	
			(if (contain? (car l2) l1) (append (list (car l2)) (intersect l1 (cdr l2))) (intersect l1 (cdr l2)))
			(if (contain? (car (list l2) l1)) (append (list (car (list l2))) (intersect l1 (cdr (list l2)))) (intersect l1 (cdr (list l2))))))))

(define union (lambda (l1 l2)
	(if (or (null? l1)(null? l2)) 
		(append l1 l2) 
		(if (contain? (car l2) l1) 
			(union l1 (cdr l2))
			(union (append l1 (list (car l2))) (cdr l2))) )))
	
	
(define notAbutB (lambda (l1 l2)
	(if 
		(null? l2) 
		'() 
		(append ( if (contain? (car l2) l1) '() (list (car l2))) (notAbutB l1 (cdr l2))))))
			
	
(define let*-free-vars-helper (lambda (slist result)
	(if 
		(null? slist) 
		'()
		(if (contain? (cadr (car slist)) result) 
			(append '() (let*-free-vars-helper (cdr slist) (append result (list (car (car slist)))))) 
			(append (list (cadr (car slist))) (let*-free-vars-helper (cdr slist) (append result (list (car (car slist))))))
		))))
	
;(car slist) = 'let'
;(cadr slist) = list of variable bounding lists
;(every-first-from-list (cadr slist)) = list of variables that should be bound
;(caddr slist) = expression	
	
(define apply-free-vars-helper-to-list (lambda (slist)
	(if (null? slist) 
		'() 
		(append (free-vars-helper (car slist)) (apply-free-vars-helper-to-list (cdr slist))))))
	
	

;[(eqv? 'let (car slist)) (difference 
;	(difference 
;		(notAbutB (every-first-from-list (cadr slist)) (flat (caddr slist))) 
;		(free-vars-helper (cadr slist)))
;	(free-vars-helper (caddr slist)))]
												
	
	
	
(define free-vars-helper (lambda (slist)
	(if (null? slist) 
		'() 
		(cond 
			[(list? slist)
				(cond 
					[(eqv? 'lambda (car slist)) (intersect (notAbutB (cadr slist) (flat (caddr slist))) (free-vars-helper (caddr slist)) )] 
					[(eqv? 'let (car slist)) (union	
												(difference 
													(difference 
														(notAbutB (every-first-from-list (cadr slist)) (flat (caddr slist))) 
														(free-vars-helper (cadr slist)))
													(free-vars-helper (caddr slist))) 
												(apply-free-vars-helper-to-list(every-second-from-list (cadr slist))) 
											)]
					[(eqv? 'let* (car slist)) (union 
													(let*-free-vars-helper (cadr slist) '())
													(notAbutB (every-first-from-list (cadr slist)) (flat (caddr slist))) 
														)]
					[(eqv? 'set! (car slist)) (free-vars-helper (caddr slist))]
					[else (append (free-vars-helper(car slist)) (free-vars-helper (cdr slist)))])
				]
			[(symbol? slist)(if 
				(eqv? 'lambda slist) '() (list slist))]
			[else '()]))))
			
(define difference (lambda (l1 l2)
	(union (difference-helper l1 l2) (difference-helper l2 l1))))
	
(define difference-helper (lambda (l1 l2)
	(if (null? l2)
		'()
		(if 
			(contain? (car l2) l1) 
			(difference-helper l1 (cdr l2)) 
			(append (list (car l2)) (difference-helper l1 (cdr l2))) ))))

(define every-first-from-list (lambda (l)
	(if (null? (cdr l)) 
		(list(car(car l))) 
		(append (list (car(car l))) (every-first-from-list (cdr l)))  ) ))
		
(define every-second-from-list (lambda (l)
	(if (null? (cdr l)) 
		(list(cadr(car l))) 
		(append (list (cadr(car l))) (every-second-from-list (cdr l)))  ) ))
		
(define free-vars (lambda (slist)
	(let ([result (free-vars-helper slist)]) 
	(no-duplicate result '())  ) ))

;Problem 3	
(define occurs-bound? (lambda (ele slist)
	(contain? ele (bound-vars slist))))
	

(define occurs-free? (lambda (ele slist)
	(contain? ele (free-vars slist))))
	
	
	
;Problem 4
(define get-index (lambda(ele lst)
	(if (null? lst)
		1
		(if (eqv? ele (car lst))
			0
			(+ 1 (get-index ele (cdr lst) ))))))
	
(define lexical-change-helper (lambda (lexical ori lst index)
	(if 
		(null? lexical)
		(let loop ([trace lst][index index])
			(if 
				(null? trace) 
				'() 
				(append (list (list (car trace) 0 (get-index (car trace) ori) )) (loop (cdr trace) (+ 1 index))))
		)
		(if (null? lst) 
			(append (list (list (car (car lexical))(+ 1 (cadr (car lexical)))(caddr (car lexical))))
					(lexical-change-helper (cdr lexical) ori ori index)) 
			(if (eqv? (car(car lexical)) (car lst))
				(append (list (list (car lst) 0 (get-index (car lst) ori))) (lexical-change-helper (cdr lexical) ori ori (+ 1 index)))
				(lexical-change-helper lexical ori (cdr lst) index))
			)
		)))
	
(define word-to-lexical (lambda (ele lexical)
	(if 
		(null? lexical)
		(list ': 'free ele)
		(if (eqv? ele (car(car lexical)))
			(list ': (cadr (car lexical)) (caddr (car lexical)))
			(word-to-lexical ele (cdr lexical))
			))))

(define lexical-address-helper (lambda (slist lexical)
	(if (null? slist)
		'()
		(cond 
			[(list? slist) 
				(cond
					[(eqv? 'lambda (car slist)) (append 
													(list 'lambda (cadr slist))
													(list (lexical-address-helper
														(caddr slist)
														(lexical-change-helper lexical (cadr slist) (cadr slist) 0))))]
					[(eqv? 'if (car slist)) (append 
												(list 'if)
												(list (lexical-address-helper (cadr slist) lexical))
												(list (lexical-address-helper (caddr slist) lexical))
												(list (lexical-address-helper (cadddr slist) lexical)))]
					[else
						(if (null? (cdr slist))
							(list (lexical-address-helper (car slist) lexical))
							(append (list(lexical-address-helper (car slist) lexical))
									(lexical-address-helper (cdr slist) lexical))) 
						]
					)]
			[(symbol? slist) (word-to-lexical slist lexical)]
			[else '()]
					))))	
					
(define lexical-address (lambda (slist)
	(lexical-address-helper slist '())))
	
(define lexical-change-helper-2 (lambda (lexical ori lst index)
	(if 
		(null? lexical)
		(let loop ([trace lst][index index])
			(if 
				(null? trace) 
				'() 
				(append (list (list (car trace) 0 (get-index (car trace) ori) )) (loop (cdr trace) (+ 1 index))))
		)
		(if (null? lst) 
			(append (list (list (car (car lexical))(+ 1 (cadr (car lexical)))(caddr (car lexical))))
					(lexical-change-helper-2 (cdr lexical) ori ori index)) 
			(lexical-change-helper-2 lexical ori (cdr lst) index))
			)
		))	

(define lexical-to-word (lambda (ele lexical)
	(if 
		(null? lexical)
		(caddr ele)
		(if (and 
				(eqv? (cadr ele)(cadr (car lexical)))
				(eqv? (caddr (car lexical)) (caddr ele)))
			(car (car lexical))
			(lexical-to-word ele (cdr lexical)))
		)))		
		
(define un-lexical-address-helper (lambda (slist lexical)
	(if (null? slist)
		'()
		(cond 
			[(list? slist) 
				(cond
					[(eqv? 'lambda (car slist)) (append 
													(list 'lambda (cadr slist))
													(list (un-lexical-address-helper
														(caddr slist)
														(lexical-change-helper-2 lexical (cadr slist) (cadr slist) 0))))]
					[(eqv? 'if (car slist)) (append 
												(list 'if)
												(list (un-lexical-address-helper (cadr slist) lexical))
												(list (un-lexical-address-helper (caddr slist) lexical))
												(list (un-lexical-address-helper (cadddr slist) lexical)))]
					[(eqv? ': (car slist)) (lexical-to-word slist lexical)]
					[else
						(if (null? (cdr slist))
							(list (un-lexical-address-helper (car slist) lexical))
							(append (list(un-lexical-address-helper (car slist) lexical))
									(un-lexical-address-helper (cdr slist) lexical))) 
						]
					)]
			[(symbol? slist) (lexical-to-word slist lexical)]
			[else '()]
					))))	
					
(define un-lexical-address (lambda (slist)
	(un-lexical-address-helper slist '())))