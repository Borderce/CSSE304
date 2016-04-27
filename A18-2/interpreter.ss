(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env (id-k))))

; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env k)
    (cases expression exp
	  [keys-list-exp (datum) (apply-k k datum)]
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id) (apply-env env id ;look up its value.
      	k ;continuation to call if id is in the environment 
        (lambda () (eopl:error 'apply-env ;called if id not in env
		   "variable not found in environment: ~s" id)))]
      [app-exp (rator rands)
        (eval-exp rator env (rator-k rands env k))]
		;(if (equal? 'closure-ref (car proc-value)) 
		;	(let ([args (eval-rands-ref rands env (2nd proc-value))])
		;	(apply-proc proc-value args))
        ;    (let ([args (eval-rands rands env)])
		;	(apply-proc proc-value args)))]
	  [quote-exp (datum) (apply-k k datum)]
	  [if-exp (test-exp then-exp else-exp)
		(eval-exp test-exp env (test-k then-exp else-exp env k))]
	  [letrec-exp (proc-names idss bodies letrec-body)
		(cond ;[(and (pair? idss)(list? idss)) (eval-bodies letrec-body (extend-env-recursively
				;				(unparse-exp-list proc-names) idss bodies env))]
			  [(and (pair? idss)(list? idss)) (eval-bodies 
												letrec-body 
												(extend-env-recursively (unparse-exp-list proc-names) idss bodies env)
												k)]
												;(letrec-k
								;(unparse-exp-list proc-names) idss (id-k (lambda (v) v)) env k))]
				[else (eopl:error 'eval-exp "Bad idss field: ~a" idss)])]
	  [letrec-exp-improper (proc-names idss bodies letrec-body)
		(eval-bodies letrec-body (extend-env-recursively-improper
								(unparse-exp-list proc-names) idss bodies env) k)]
	  ;[let-exp (vars exps bodies)
			;(eval-bodies 
			;	bodies 
			;	(extend-env (unparse-exp-list vars) (eval-rands exps env ) env) k)
			;(map-cps (lambda (x k2) (eval-exp x env k2)) exps (let-k env bodies vars k))]
	  ;[while-exp (test-exp then-exp) (letrec ([loop (lambda ()
	   ;(if (eval-exp test-exp env) (begin (eval-exp (syntax-expand then-exp) env) (loop))
	   ;(lit-exp (list (void)))))]) (loop))]
	  [lambda-exp-ref (vars bodies) (closure-ref vars bodies env)]
	  [lambda-exp (vars bodies) (cond
			[(expression? vars) ;(unparse-exp vars (closure-k bodies env k))]
			(apply-k k (closure (unparse-exp vars) bodies env))]
			[((list-of expression?) vars)(apply-k k (closure (unparse-exp-list vars) bodies env))]
			[else (display vars)] )]
	  [lambda-exp-improper (vars improper-var bodies) 
	   (apply-k k (closure-improper (unparse-exp-list vars) (unparse-exp improper-var) bodies env))]
	  [set!-exp (id exp)	(apply-env-ref 
								env 
								(unparse-exp id) 
								(set!-k exp env k)
								(lambda () (eopl:error 'apply-env ;called if id not in env
								"variable not found in environment (set!): ~s" id))) ]
	  [define-exp (id body) ;(add-global id (eval-exp (syntax-expand body) env k))]
		(eval-exp body env (add-global-k id k))]
	 ; [or-exp (exps) (eval-exp (or-expand exps) env)]
	  ;[cond-exp (exps) (eval-exp (cond-expand exps) env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))
	  
; evaluate the list of operands, putting results into a list
(define eval-bodies (lambda (bodies env k) 
	(if (null? (cdr bodies))
		(eval-exp (car bodies) env k)
		(eval-exp (car bodies) env (ev-bodies-k (cdr bodies) env k)))))

(define eval-rands-ref (lambda (rands env vars)
	(cond [(null? (cdr rands)) (list (if (list? (car vars))
		(apply-env-ref env (cadr (car rands)) ;look up its value.
      	(lambda (x) x) ;procedure to call if id is in the environment 
        (lambda () (eopl:error 'apply-env ;called if id not in env
		   "variable not found in environment: ~s" rands)))
		(eval-exp (car rands) env)))]
	[else (if (list? (car vars))
		(cons (apply-env-ref env (cadr (car rands)) ;look up its value.
      	(lambda (x) x) ;procedure to call if id is in the environment 
        (lambda () (eopl:error 'apply-env ;called if id not in env
		   "variable not found in environment: ~s" rands))) 
		   (eval-rands-ref (cdr rands) env (cdr vars)))
		(cons (eval-exp (car rands) env) (eval-rands-ref (cdr rands) env (cdr vars))))])))
			
(define eval-rands
  (lambda (rands env k)
   ;(map-cps (lambda (x k2) (apply-k k2 (eval-exp x env (id-k (lambda (v) v)))))  rands k)))
   (map-cps (lambda (x k2) (eval-exp x env k2))  rands k)))
   
(define map-cps
	(lambda (cps-proc lst k)
		(if (null? lst)
			(apply-k k '())
			(cps-proc (car lst) (map-k cps-proc (cdr lst) k)))))
			
(define map-cps-prim
	(lambda (cps-proc lst k)
		(if (null? lst)
			(apply-k k '())
			(apply-proc cps-proc (list (car lst)) (map-k-prim cps-proc (cdr lst) k)))))
			
(define make-improper (lambda (args len index)
	(cond [(<= index len) (cons (car args) 
							(make-improper (cdr args) len (+ index 1)))]
		  [else (list args)])))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.		  
(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
			; You will add other cases
	  [closure (vars bodies env) (cond [((list-of symbol?)vars)
	  (eval-bodies bodies (extend-env vars args env) k)]
		[else (eval-bodies bodies (extend-env (list vars) (list args) env) k)])]
	  [closure-ref (vars bodies env) (eval-bodies bodies (extend-env 
	  (remove-ref vars) args env) k)]
	  [closure-improper (vars improper-var bodies env) 
	   (eval-bodies bodies (extend-env (append vars (list improper-var)) 
	      (make-improper args (length vars) 1) env) k)]
	  [continuation-proc (k3) (apply-k k3 (car args))]
	  [exit-proc () (apply-k (exit-k) args)]
	  ;[continuation-proc (k) (apply-k k (car args))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
				
(define remove-ref (lambda (vars) (cond [(null? (cdr vars))
	(list (if (list? (car vars)) (cadr (car vars)) (car vars)))]
	[else (cons (if (list? (car vars)) (cadr (car vars)) (car vars)) 
	(remove-ref (cdr vars)))])))

(define *prim-proc-names* ;'(map +))
 '(+ - * / sub1 add1 cons = < <= >= > not zero? reverse exit-list
 car cdr list list? null? eq? equal? eqv? length pair? atom? list-vector 
 procedure? vector vector-list vector? make-vector vector-ref number? 
 symbol? set-car! set-cdr! vector-set! display newline caaar caadr caar cadar
 caddr cadr cdaar cdadr cddar cdddr cddr list->vector vector->list vector 
 vector? make-vector vector-ref number? symbol? set-car! set-cdr! call/cc
 vector-set! display newline apply map quotient memv append list-tail assq))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))
(define global-env (cons '() (vector))) ;global-env is dynamic env for syms and vals
;vals should be in cells so that they can be mutated like other envs.
;Initialize to null so that I can check if its empty or not.
(define reset-global-env (lambda () (set! global-env (cons '() (vector)))))
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args k) (cond [(null? prim-proc) (error 'apply-prim-proc
    "You have no primitive procedure specified")]
	[(null? args) (error 'apply-prim-proc "You have no arguments specified")]
	[else (case prim-proc 
	  [(exit-list) (apply-proc (exit-proc) args k)]
	  [(call/cc) (apply-proc (car args) (list (continuation-proc k)) k)]
	  [(map) (map-cps-prim (car args) (cadr args) k)]
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
	  [(reverse) (apply-k k (reverse (1st args)))]
	  [(add1) (if (= 1 (length args)) (apply-k k (+ 1 (1st args))) 
	  (apply-k k (error 'apply-prim-proc
		"You need to pass one argument into add1, args: " args)))]
      [(sub1) (if (= 1 (length args)) (apply-k k (- (1st args) 1)) 
	  (apply-k k (error 'apply-prim-proc
		"You need to pass one argument into sub1, args: " args)))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(=) (apply-k k (apply = args))]
      [(<) (apply-k k (apply < args))]
      [(<=) (apply-k k (apply <= args))]
      [(>=) (apply-k k (apply >= args))]
      [(>) (apply-k k (apply > args))]
      [(not) (apply-k k (not (1st args)))]
      [(zero?) (apply-k k (= 0 (1st args)))]
      [(car) (apply-k k (car (1st args)))] [(cadr) (apply-k k (car (cdr (1st args))))]
	  [(caddr) (apply-k k (car (cdr (cdr (1st args)))))] [(cdr) (apply-k k (cdr (1st args)))]
	  [(cddr) (apply-k k (cdr (cdr (1st args))))] [(cdddr) (apply-k k (cdr (cdr (cdr (1st args)))))]
	  [(caar) (apply-k k (car (car (1st args))))] [(caaar) (apply-k k (car (car (car (1st args)))))]
	  [(caadr)(apply-k k  (car (car (cdr args))))] [(cadar) (apply-k k (car (cdr (car (1st args)))))]
	  [(cdaar) (apply-k k (cdr (car (car (1st args)))))] [(cdadr) (apply-k k (cdr (car (cdr (1st args)))))]
	  [(cddar) (apply-k k (cdr (cdr (car (1st args)))))]
      [(list) (apply-k k (apply list args))]
	  [(list?) (apply-k k (apply list? args))]
	  [(quotient) (apply-k k (quotient (1st args) (2nd args)))]
      [(null?) (apply-k k (apply null? args))]
      [(eq?) (if (null? (cdr args)) (apply-k k (error 'apply-prim-proc "eq? requires 2 args"))
		(apply-k k (eq? (1st args) (2nd args))))]
      [(equal?) (if (null? (cdr args)) (apply-k k (error 'apply-prim-proc "eq? requires 2 args"))
		(apply-k k (equal? (1st args) (2nd args))))]
      [(eqv?) (if (null? (cdr args)) (apply-k k (error 'apply-prim-proc "eq? requires 2 args"))
		(apply-k k (eqv? (1st args) (2nd args))))]
		[(length) (apply-k k (apply length args))]
		[(pair?) (apply-k k (apply pair? args))]
		[(atom?) (apply-k k (not (pair? args)))]
		[(procedure?) (apply-k k (apply proc-val? args))]
		[(list->vector) (apply-k k (apply list->vector args))]
		[(vector->list) (apply-k k (apply vector->list args))]
		[(vector) (apply-k k (apply vector args))]
		[(vector-list) (apply-k k (vector-list args))]
		[(vector?) (apply-k k (apply vector? args))]
		[(make-vector) (if (number? (1st args)) (if (null? (cdr args))
		 (apply-k k (make-vector (1st args))) (apply-k k (make-vector (1st args) (2nd args))))
		 (apply-k k (error 'apply-prim-proc "First argument to make-vector must be a number")))]
		[(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
		[(number?) (if (= 1 (length args)) (apply-k k (number? (1st args))) (apply-k k (error 
		 'apply-prim-proc "number? can only be applied to an arg of length 1, not arg: " arg)))]
		[(symbol?) (if (= 1 (length args)) (apply-k k (symbol? (1st args))) (apply-k k (error
		 'apply-prim-proc "symbol? can only be applied to an arg of length 1, not arg: " arg)))]
		[(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
		[(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
		[(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
		[(display) (apply-k k (apply display args))]
		[(newline) (apply-k k (newline))]
		[(apply) (apply-k k (apply-proc (car args) (cadr args) k))]
		[(memv) (apply-k k (memv (1st args) (2nd args)))]
		[(append) (apply-k k (append (1st args)(2nd args)))]
		[(list-tail) (apply-k k (list-tail (1st args)(2nd args)))]
		[(assq)(apply-k k (assq (1st args) (2nd args)))]
	  [else (apply-k k (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc))])])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.
 
(define begin-loop (lambda (exps)
	(let loop ([exps exps])
		(if (null? (cdr exps))
			(syntax-expand (app-exp (lambda-exp (list) (list (car exps))) (list)))
			(syntax-expand (app-exp (lambda-exp (list (var-exp '_)) (list (car exps))) 
			(list (loop (cdr exps))))))))) 

(define cond-expand (lambda (exps) (let loop ([exps exps])
	(if (null? (cdr exps))
		(cadr (car exps))
		(if-exp (syntax-expand (car (car exps))) 
				(syntax-expand(cadr (car exps))) 
				(syntax-expand(loop (cdr exps))))))))

(define case-expand (lambda (eva cases) (let loop ([cases cases])
	(if (null? (cdr cases))
		(cadr (car cases))
		(if-exp (app-exp (parse-exp 'memv) (list (syntax-expand eva) (car (car cases)))) 
				(syntax-expand (cadr (car cases))) 
				(syntax-expand(loop (cdr cases))))))))
(define and-expand (lambda (exps)(let loop ([exps exps])
	(if (null? exps) (parse-exp #t) (if (null? (cdr exps))
		(car exps)
		(if-exp (syntax-expand (car exps))
				(loop (cdr exps))
				(lit-exp #f))))
	))
)
	
(define or-expand (lambda (exps)(let loop ([exps exps])
	(if (null? exps) (parse-exp #f) (if (null? (cdr exps))
		(syntax-expand (car exps))
		(syntax-expand(list 
			'let-exp 
			(list (parse-exp '_))
			(list(syntax-expand (car exps)))
			(list(if-exp 
				(parse-exp '_)
				(parse-exp '_)
				(loop (cdr exps))))))))
	))
) 
				
(define let*-expand (lambda (vars exps bodies)
	(let loop ([vars vars] [exps exps])
		(if (or (null? (cdr exps))(null? (cdr vars)))
			(syntax-expand (let-exp (list(car vars))(list (car exps)) bodies))
			(syntax-expand (let-exp (list(car vars))(list (car exps)) (list (loop (cdr vars) (cdr exps)))))))))

(define improper-expand (lambda (idss bodies)
	(let loop ([lst idss][blst bodies][resultID '()][resultBody '()])
		(if (or (null? lst) (null? blst))
			(list resultID resultBody)
			(if (and (not (list? (1st lst)))(pair? (1st lst)))
				(loop (cdr lst)(cdr blst) (cons (1st lst) resultID) (cons (1st blst) resultBody))
				(loop (cdr lst)(cdr blst) resultID resultBody))
		))))
	
(define proper-expand (lambda (idss bodies)
	(let loop ([lst idss][blst bodies][resultID '()][resultBody '()])
		(if (or (null? lst) (null? blst))
			(list resultID resultBody)
			(if (and (list? (1st lst))(pair? (1st lst)))
				(loop (cdr lst)(cdr blst) (cons (1st lst) resultID) (cons (1st blst) resultBody))
				(loop (cdr lst)(cdr blst) resultID resultBody)))
		)))
			
(define syntax-expand (lambda (e)
	(cases expression e
		[let-exp (vars exps bodies) 
		 (app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand exps))]
		[letrec-exp (proc-names idss bodies letrec-body)
					(cond [((list-of (list-of symbol?)) idss)
							(list 'letrec-exp proc-names idss (map syntax-expand bodies)(map syntax-expand letrec-body))]
						[else (list 'letrec-exp-improper proc-names idss (map syntax-expand bodies)(map syntax-expand letrec-body))])]
				
		[begin-exp (exps) (begin-loop (reverse exps))]
		[if-exp (test-exp then-exp else-exp)
		 (if-exp (syntax-expand test-exp) (syntax-expand then-exp) 
		 (syntax-expand else-exp))]
		[cond-exp (exps) (cond-expand exps)]
		[let*-exp (vars exps bodies) (let*-expand vars exps bodies)]
		[case-exp (eva cases) (case-expand eva cases)]
		[and-exp (exps) (and-expand exps)]
		[or-exp (exps) (or-expand exps)]
		[define-exp (id body) (list 'define-exp id (syntax-expand body))]
		[lambda-exp (vars bodies) (list 'lambda-exp vars (map syntax-expand bodies))]
		[app-exp (rator rands) 
				(list 'app-exp (syntax-expand rator)
						(if ((list-of expression?) rands)
							(map syntax-expand rands)
							(syntax-expand rands)))]
		[set!-exp (id exps)(list 'set!-exp id (syntax-expand exps))]
		[else e])))