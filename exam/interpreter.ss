; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env env id ;look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
	  [quote-exp (datum) datum]
	  [if-exp (test-exp then-exp else-exp) 
		(if (eval-exp test-exp env)
			(eval-exp then-exp env)
			(eval-exp else-exp env))]
	  [let-exp (vars exps bodies) 
		(let ([new-env (extend-env (unparse-exp-list vars) (eval-rands exps env) env)])
			(eval-bodies bodies new-env))]
	  [lambda-exp (vars bodies)
		(closure (unparse-exp-list vars) bodies env)]
	  [repeat-exp (exps)
		(eval-bodies exps env)]
	  [repeat-exp-l (times exps)
		(let loop ((num (eval-exp times env))) (if (eqv? 0 num)
											(void)
											(begin (eval-exp exps env) (loop (- num 1)))))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
(define eval-bodies (lambda (bodies env) 
	(let loop ([bodies bodies])
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env)
			(begin 
				(eval-exp (car bodies) env)
				(loop (cdr bodies)))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
	  [closure (vars bodies env) (eval-bodies bodies (extend-env vars args env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / sub1 add1 cons = < <= >= > not zero? 
  car cdr list list? null? eq? equal? eqv? length pair? atom? list-vector 
  vector-list procedure? vector vector-list vector? make-vector vector-ref number? 
  symbol? set-car! set-cdr! vector-set! display newline caaar caadr caar cadar
  caddr cadr cdaar cdadr cddar cdddr cddr list->vector vector->list vector 
  vector-list vector? make-vector vector-ref number? symbol? set-car! set-cdr! 
  vector-set! display newline))
(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args) (cond [(null? prim-proc) (error 'apply-prim-proc
    "You have no primitive procedure specified")]
	[(null? args) (error 'apply-prim-proc "You have no arguments specified")]
    [else (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
	  [(add1) (if (= 1 (length args)) (+ 1 (1st args)) (error 'apply-prim-proc
		"You need to pass one argument into add1, args: " args))]
      [(sub1) (if (= 1 (length args)) (- (1st args) 1) (error 'apply-prim-proc
		"You need to pass one argument into sub1, args: " args))]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(>) (apply > args)]
      [(not) (not (1st args))]
      [(zero?) (= 0 (1st args))]
      [(car) (car (1st args))] [(cadr) (car (cdr (1st args)))]
	  [(caddr) (car (cdr (cdr (1st args))))] [(cdr) (cdr (1st args))]
	  [(cddr) (cdr (cdr (1st args)))] [(cdddr) (cdr (cdr (cdr (1st args))))]
	  [(caar) (car (car (1st args)))] [(caaar) (car (car (car (1st args))))]
	  [(caadr) (car (car (cdr args)))] [(cadar) (car (cdr (car (1st args))))]
	  [(cdaar) (cdr (car (car (1st args))))] [(cdadr) (cdr (car (cdr (1st args))))]
	  [(cddar) (cdr (cdr (car (1st args))))]
      [(list) (apply list args)]
	  [(list?) (apply list? args)]
      [(null?) (apply null? args)]
      [(eq?) (if (null? (cdr args)) (error 'apply-prim-proc "eq? requires 2 args")
		(eq? (1st args) (2nd args)))]
      [(equal?) (if (null? (cdr args)) (error 'apply-prim-proc "eq? requires 2 args")
		(equal? (1st args) (2nd args)))]
      [(eqv?) (if (null? (cdr args)) (error 'apply-prim-proc "eq? requires 2 args")
		(eqv? (1st args) (2nd args)))]
		[(length) (apply length args)]
		[(pair?) (pair? args)]
		[(atom?) (not (pair? args))]
		[(procedure?) (apply proc-val? args)]
		[(list->vector) (apply list->vector args)]
		[(vector->list) (apply vector->list args)]
		[(vector) (apply vector args)]
		[(vector-list) (vector-list args)]
		[(vector?) (apply vector? args)]
		[(make-vector) (if (number? (1st args)) (if (null? (cdr args))
		 (make-vector (1st args)) (make-vector (1st args) (2nd args)))
		 (error 'apply-prim-proc "First argument to make-vector must be a number"))]
		[(vector-ref) (vector-ref (1st args) (2nd args))]
		[(number?) (if (= 1 (length args)) (number? (1st args)) (error 
		 'apply-prim-proc "number? can only be applied to an arg of length 1, not arg: " arg))]
		[(symbol?) (if (= 1 (length args)) (symbol? (1st args)) (error
		 'apply-prim-proc "symbol? can only be applied to an arg of length 1, not arg: " arg))]
		[(set-car!) (set-car! (1st args) (2nd args))]
		[(set-cdr!) (set-cdr! (1st args) (2nd args))]
		[(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
		[(display) (display args)]
		[(newline) (newline)]
	  [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])])))


(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










