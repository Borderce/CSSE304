;; Parsed expression datatypes
(define-datatype expression expression?
	[var-exp        ; variable references
		(id symbol?)]
	[lit-exp        ; "Normal" data.  Did I leave out any types?
		(datum
			(lambda (x)
				(ormap (lambda (pred) (pred x))
				(list number? vector? boolean? symbol? string? pair? null?))))]
	[app-exp        ; applications
		(rator expression?)
		(rands (lambda (x) (or (expression? x)((list-of expression?) x))))]  
	[quote-exp
		(datum always?)]
	[if-exp
		(test-exp expression?)
		(then-exp expression?)
		(else-exp always?)]
	[let-exp
		(vars (list-of expression?))
		(exps (list-of expression?))
		(bodies (list-of expression?))]
	[let*-exp
		(vars (list-of expression?))
		(exps (list-of expression?))
		(bodies (list-of expression?))]
	[lambda-exp
		(vars (lambda (x) (or (expression? x)((list-of expression?) x))))
		;(lambda (x) (or (symbol? x)((list-of symbol?) x)))
		(bodies (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[lambda-exp-improper ;improper means (lambda (x y . z)
		(vars (list-of expression?))
		(extra-vars symbol?)
		(bodies (list-of expression?))]
	[lambda-exp-ref ;for reference for pass-by-ref
		(vars list?)
		(bodies (lambda (x) (or (expression? x) ((list-of expression?) x))))]
	[begin-exp
		(exps (list-of expression?))]
	[set!-exp
		(var (expression?))
		(ex (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[cond-exp
		(exps (list-of expression?))]
	[while-exp
		(test-exp (lambda (x) (or (expression? x)((list-of expression?) x))))
		(then-exp expression?)]
	[case-exp
		(eva (lambda (x) (or (expression? x)((list-of expression?) x))))
		(cases (list-of expression?))]
	[keys-list-exp
		(keys (list-of always?))]
	[and-exp 
		(exps (list-of expression?))]
	[or-exp 
		(exps (list-of expression?))]
	[letrec-exp
		(proc-names (list-of symbol?))
		(idss (lambda (x) (or (list-of (list-of symbol?)))))
		(bodies (list-of expression?))
		(letrec-body (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[letrec-exp-improper
		(proc-names (list-of symbol?))
		(idss (lambda (x) (or (list-of (list-of symbol?)))))
		(bodies (list-of expression?))
		(letrec-body (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[define-exp
		(id symbol?)
		(body always?)]
)
	
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (lambda (x) (or (list-of scheme-value?) (pair? x))))
   (env environment?))
  (recursively-extended-env-record
	(proc-names (list-of symbol?))
	(idss (lambda (x) (or (list-of (list-of symbol?))((list-of pair?) x))))
	(bodies (list-of expression?))
	(env environment?))
  (recursively-extended-env-record-improper
	(proc-names (list-of symbol?))
	(idss (lambda (x) (or (list-of (list-of symbol?))((list-of pair?) x))))
	(bodies (list-of expression?))
	(env environment?)))
	
(define-datatype continuation continuation?
	[test-k	(then-exp expression?)
		(else-exp always?)
		(env environment?)
		(k continuation?)]
	[rator-k (rands (list-of expression?))
			(env environment?)
			(k continuation?)]
	[rands-k (proc-value scheme-value?)
			(k continuation?)]
	[id-k (proc always?)]
	[ev-bodies-k (cdrbodies (list-of expression?))
		(env environment?)
		(k continuation?)]
	[letrec-k 
		(proc (list-of symbol?))
		(idss (lambda (x) (or (list-of (list-of symbol?)))))
		(vars (list-of expression?))
		(env environment?)
		(k continuation?)]
	[set!-k 
		(exp expression?)
		(env environment?)
		(k continuation?)]
	[set!-k2
		(cell cell?)
		(k continuation?)])
	
(define apply-k
	(lambda (k val)
		(cases continuation k
			[id-k (proc) (proc val)]
			[test-k (then-exp else-exp env k)
				(if val (eval-exp then-exp env k)
						(eval-exp else-exp env k))]
			[rator-k (rands env k)
					(eval-rands rands env (rands-k val k))]
			[rands-k (proc-value k)
					(apply-proc proc-value val k)]
			[ev-bodies-k (cdrbodies env k)
				(eval-bodies cdrbodies env k)]
			[letrec-k (proc-names idss bodies env k)
				(eval-bodies bodies env k)]
			[set!-k (exp env k)
				(eval-exp exp env (set!-k2 val k))]
			[set!-k2 (cell k)
				(apply-k k (set-car! cell val))])))
	
;(if (equal? 'closure-ref (car proc-value)) 
		;	(let ([args (eval-rands-ref rands env (2nd proc-value))])
		;	(apply-proc proc-value args))
        ;    (let ([args (eval-rands rands env)])
		;	(apply-proc proc-value args)))]
		
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
	(vars   (lambda (x) (or (symbol? x)((list-of symbol?) x)(pair? x))) )
	(bodies (list-of expression?))
	(env environment?)]
   [closure-improper
	 (vars (list-of symbol?))
	 (improper-var symbol?)
	 (bodies (list-of expression?))
	 (env environment?)]
	[closure-ref
		(vars list?)
		(bodies (list-of expression?))
		(env environment?)]
)
	
;; environment type definitions
(define scheme-value?
  (lambda (x) #t))
