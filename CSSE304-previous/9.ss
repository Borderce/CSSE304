;Bo Peng Assignment 9

;Program 1(a)
(define snlist-recur
	(lambda (base-value list-proc other-proc)
		(letrec
			([helper
				(lambda (sn-list)
					(cond [(null? sn-list) base-value]
						[(list? sn-list) (if (list?(car sn-list)) 
											(list-proc (helper (car sn-list))(helper (cdr sn-list))) 
											(other-proc (car sn-list)(helper (cdr sn-list))))]
						[ (or(number? (car sn-list))(symbol? (car sn-list))) sn-list]
						[else base-value]
						))])
			helper)))
			
(define sn-list-sum (lambda (snlst)
	((snlist-recur '0 + +) snlst)))

;Program 1(b)
(define sn-list-map (lambda (proc snlst)
	((snlist-recur '() (lambda (first last) 
	(cond 	
			[(list? first) (cons first last)]
			[else (cons (proc first) last)])) (lambda (first last) 
	(cond 	
			[(list? first) (cons first last)]
			[else (cons (proc first) last)]))) snlst)))

;Program 1(c)
(define sn-list-paren-count (lambda (snlst)
	(+ 2((snlist-recur '0 (lambda (first last) (+ 2 first last)) 
	(lambda (first last) (+ 0 last)))  snlst))))


;Program 1(d)
(define sn-list-reverse (lambda (snlst)
	(reverse ((snlist-recur '() (lambda (first last) 
									(cond [(list? first) (cons(reverse first) last)] 
											[else (cons first last)])) cons) snlst))))
					
;Program 1(e)
(define sn-list-occur (lambda (s snlst)
	((snlist-recur '0 (lambda (first last) (+ first last)) 
	(lambda (first last) (cond [(eqv? s first) (+ 1 last)] [else (+ 0 last)]))) snlst)))
	
;Program 1(f)
(define sn-list-depth (lambda (snlst)
	 (+ 1((snlist-recur '0 (lambda (first last) (max (+ 1 first) last)) (lambda (first last) (max 0 last)) )snlst))))
	 
;Program 2
(define bt-recur
	(lambda (base-value lr-proc ele-proc)
		(letrec
			([helper
				(lambda (T)
					(cond [(null? T) base-value]
						[(list? T) (cond [(and (list?(cadr T))(list? (caddr T))) (lr-proc (ele-proc(car T)) (helper(cadr T)) (helper(caddr T)))] 
											[(and(list?(cadr T))(not (list?(caddr T)))) (lr-proc (ele-proc(car T)) (helper(cadr T)) (ele-proc(helper(caddr T)))) ]
											[(and(not (list?(cadr T))) (list?(caddr T))) (lr-proc (ele-proc(car T)) (ele-proc(helper(cadr T))) (helper(caddr T)))]
											[else (lr-proc (ele-proc(car T)) (ele-proc(helper(cadr T))) (ele-proc(helper(caddr T))))])]
						[ (or(number? T)(symbol? T)) T]
						[else base-value]
						))])
			helper)))
			
(define bt-sum (lambda (T)
	((bt-recur '0 (lambda (root left right) (+ left right)) (lambda (x) x)) T)))

(define bt-inorder (lambda (T)
	(if (not (list? T)) '() ((bt-recur '() (lambda (root left right)  (cond [ (and(not (list? left))(not(list? right))) (list root) ]
												[(and(not (list? left))(list? right))  (append (list root) right) ]
												[(and(list? left)(not(list? right))) (append left (list root))]
												[else (append left (list root) right)]))  
		(lambda (x) x ) ) T) )
	))

