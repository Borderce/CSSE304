;Problem 1a
(define (slist-map proc slist) 
  (let notate ([slist slist])
    (cond [(null? slist) '()]
  	  [(symbol? (car slist))  (cons (proc (car slist)) 
					(notate (cdr slist)))]
	  [else (cons (notate (car slist)) 
		      (notate (cdr slist)))]
			  )))
			
;Problem 1b
(define (slist-reverse slist)
	(reverse (map (lambda (sl) (cond [(list? sl) (reverse sl)]
										   [else sl])) slist)))
										   
;Problem 1c
(define (count-list slist)
	(if 
		(symbol? slist) 
		'0 
		(let count-recur ([slist slist] [len (length slist)])
		(cond [(null? slist) len]
			  [(symbol? (car slist)) (count-recur (cdr slist) (- len 1)) ]
			  [else (count-recur (cdr slist) len)])))
)

(define (slist-paren-count-recur slist)
	(if 
		(eqv? 0 (count-list slist)) 
		'0
		(+ (count-list slist) (apply + (map slist-paren-count-recur slist)) )) 
)

(define (slist-paren-count slist)
	 (* 2 (+ 1 (slist-paren-count-recur slist))))

;Problem 1d
(define (slist-depth slist)
	(if 
		(eqv? 0 (count-list slist)) 
		'1
		(+ 1  (apply max (map slist-depth slist))) ))

;Program 1e
(define app-ele (lambda (result ls)
	(if (null? ls) 
		result 
		(if (symbol? (car ls)) 
			(app-ele (append result (list (car ls))) (cdr ls))
			(app-ele result (cdr ls))))
	))

	
(define app-list (lambda (result ls)
	(if (null? ls) 
		result 
		(if (list? (car ls)) 
			(app-list (append result (list (car ls))) (cdr ls))
			(app-list result (cdr ls))))
	))

(define copy-list (lambda (obj d)
	(if (zero? d) 
	'()  
	(cons obj (copy-list obj (- d 1)) ) )))

(define  (flat slist)
    (let flat ([slist slist])
      (cond [(null? slist) '()]
	    [(symbol? (car slist)) (cons (car slist)
					 (flat (cdr slist)))]
	    [else (append (flat (car slist)) (flat (cdr slist)))])))	

(define (slist-symbols-at-depth slist d)
	(let inner-loop ([cd d] [slist slist]) 
		(if  (eqv? 0 (- cd 1)) 
			(app-ele '() slist) 
			(flat (map inner-loop (copy-list (- cd 1) (length(app-list '() slist))) (app-list '() slist))) )))
			
;Program 2	
(define contain? (lambda (e l)
	(if (null? l) #f (if (eqv? e (car l)) #t (or #f (contain? e (cdr l)))))))
	
(define subst-leftmost (lambda (new old slist equality-pred?)
	(if (null? slist) 
		'()
		(if (or (contain? old slist) (> (length (app-list '() slist)) 0))
			(let sub-helper ([current (car slist)] [left (cdr slist)]) 
						 (if (or (symbol? current) (null? current))
							(if 
								(equality-pred? current old) 
								(cons new left) 
								(cons current(sub-helper (car left) (cdr left)) )) 
							(cons (sub-helper (car current) (cdr current)) left)))
			'()) )))

