; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define make-non-ref-cells (lambda (vals)
	(cond [(null? vals) vals]
	[(null? (cdr vals)) (list (if (cell? (car vals)) (car vals) (cell (car vals))))]
	[else (cons (if (cell? (car vals)) (car vals) (cell (car vals))) 
		(make-non-ref-cells (cdr vals)))])))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (make-non-ref-cells vals) env))) ;in day 30 of the slides,
	;what this will do is instead of just sending the values, such as (1 2 3),
	;we will instead send cells such as (1 . 'this-is-a-cell), (2 . 'this-is-a-cell),
	;(3 . 'this-is-a-cell).  Then, we will use cell-ref to get the value in the cell.
	;We need to implement cells in order to make the environment mutatable for set!.

(define extend-env-recursively
	(lambda (proc-names idss bodies old-env)
		(recursively-extended-env-record proc-names idss bodies old-env)))
	
(define extend-env-recursively-improper
	(lambda (proc-names idss bodies old-env)
		(recursively-extended-env-record-improper proc-names idss bodies old-env)))
	
(define extend-env-improper
  (lambda (syms impropersym vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))
(define get-improper-last (lambda (lst)
	(if (symbol? lst)
		lst
		(get-improper-last (cdr lst)))))
		
(define get-improper-first (lambda (lst)
	(if (symbol? lst)
		'()
		(cons (car lst)(get-improper-first (cdr lst))))))
	
(define add-global (lambda (sym val)
	(begin (set-car! global-env (cons sym (car global-env)))
	(set-cdr! global-env (list->vector (cons (cell val) (vector->list (cdr global-env))))))))
	
(define look-global (lambda (carg cdrg sym succeed fail)
	(let ((pos (list-find-position sym carg)))
      	  (if (number? pos)
		  (apply-k succeed (cell-ref (vector-ref cdrg pos)))
	      (fail)))))
(define look-global-ref (lambda (carg cdrg sym succeed fail)
	(let ((pos (list-find-position sym carg)))
      	  (if (number? pos)
		  (apply-k succeed (vector-ref cdrg pos))
	      (fail)))))		  
		  
(define set!-global (lambda (carg cdrg sym succeed fail)
	(let ((pos (list-find-position sym carg)))
      	  (if (number? pos)
		  (apply-k succeed (vector-ref cdrg pos))
	      (fail)))))
		  
(define apply-env-ref (lambda (env sym succeed fail)
	(cases environment env
      (empty-env-record ()
        (set!-global (car global-env) (cdr global-env) sym succeed fail))
		;find sym in global to return ref to val
      (extended-env-record (syms vals env) ;replaces the arguments with the paramaters passed in
		(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
		  (apply-k succeed (list-ref vals pos))
	      (apply-env-ref env sym succeed fail))))
	  (recursively-extended-env-record (procnames idss bodies old-env)
		(let 
			([pos (list-find-position sym procnames)])
			(if (number? pos)
				(apply-k(closure 
					(list-ref idss pos)
					(list(list-ref bodies pos))
					env))
				(apply-env-ref old-env sym succeed fail))))
		(recursively-extended-env-record-improper (procnames idss bodies old-env)
		 sym ) ;need to change this later possibly
		 )))
	
(define apply-env
  (lambda (env sym succeed fail)  ; succeed and fail are procedures applied if the var 
  ;is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (look-global (car global-env) (cdr global-env) sym succeed fail)) ;look in dynamic global if not here
      (extended-env-record (syms vals env) ;replaces the arguments with the paramaters passed in
		(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
		  (apply-k succeed (cell-ref (list-ref vals pos)))
	      (apply-env env sym succeed fail))))
	  (recursively-extended-env-record (procnames idss bodies old-env)
		(let 
			([pos (list-find-position sym procnames)])
			(if (number? pos)
				(apply-k succeed (closure 
					(list-ref idss pos)
					(list(list-ref bodies pos))
					env))
				(apply-env old-env sym succeed fail))))
	  (recursively-extended-env-record-improper (procnames idss bodies old-env)
		(let 
			([pos (list-find-position sym procnames)])
			(if (number? pos)
				(if (and (not (list? (list-ref idss pos)))(pair? (list-ref idss pos)))
					(apply-k succeed (closure-improper 
						(get-improper-first(list-ref idss pos))
						(get-improper-last(list-ref idss pos))
						(list(list-ref bodies pos))
						env))
					(apply-k succeed (closure 
						(list-ref idss pos)
						(list(list-ref bodies pos))
						env)))
				(apply-env old-env sym succeed fail)))))))

(define cell-set! set-car!) (define set-ref! cell-set!)
(define cell (lambda (val) (cons val 'this-is-a-cell)))
(define cell-ref car) (define deref cell-ref) 
(define cell? (lambda (obj) (and (pair? obj) (eq? (cdr obj) 'this-is-a-cell))))