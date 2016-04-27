
;; Parsed expression datatypes
;(define-syntax repeat 
;	(syntax-rules ()
;		[(_ count-exp body1 body2 ...)
;		()]
;	))

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]  
  [quote-exp
	(datum always?)]
  [if-exp
	(test-exp expression?)
	(then-exp expression?)
	(else-exp expression?)]
  [let-exp
	(vars (list-of expression?))
	(exps (list-of expression?))
	(bodies (list-of expression?))]
  [lambda-exp
	(vars (list-of expression?))
	(bodies (list-of expression?))]
  [repeat-exp
	(bodies (list-of expression?))]
  [repeat-exp-l
	(times (list-of expression?))
	(bodies (list-of expression?))]
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
   
   
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
	(vars (list-of symbol?))
	(bodies (list-of expression?))
	(env environment?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
