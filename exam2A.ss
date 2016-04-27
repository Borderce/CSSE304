;Problem 1
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define BST-height-sum (lambda (lst)
	(2nd (BST-height-sum-helper lst))))
	
(define BST-height-sum-helper (lambda (lst)
	(if (list? lst)
		(if (null? lst)
			(list 0 0)
			(let (
				(left (BST-height-sum-helper(2nd lst)))
				(right (BST-height-sum-helper(3rd lst)))
				)
				(get-new left right)))
		(list 0 0))))
		
(define get-new (lambda (left right)
	(let 
		([sum (+ (2nd left) (2nd right))]
		[n (max (1st left) (1st right))]
		)
		(list (+ 1 n) (+ 1 n sum))
		)))
		
;Problem 2
(define-syntax simple-loop
	(syntax-rules (from to)
		[
			(_ v from start to end e1 ...)
			(let loop 
				([s start][e end])
				(let [(v s)] e1 ... 
							(if (< s e)
								(loop (+ 1 s) e)
								))
			)
		]))

;Problem 3
(define change-mset (lambda (mset sym)
	(if (null? mset) 
		(list (list sym 1))
		(if (eqv? (1st (1st mset)) sym)
			(cons (list sym (+ 1 (2nd (1st mset)))) (cdr mset))
			(cons (1st mset) (change-mset (cdr mset) sym))
		))
	)
)

(define count-sym (lambda (mset sym)
	(if 
		(null? mset) 
		0 
		(if (eqv? (1st (1st mset)) sym)
			(2nd (1st mset))
			(count-sym (cdr mset) sym)))
	))

(define list->mset (lambda (lst mset)
	(if (null? lst)
		mset
		(list->mset (cdr lst)(change-mset mset (car lst))))))
		
(define mset->list (lambda (mset)
	(if (null? mset)
		'()
		(let loop ([num (2nd (1st mset))])
			(if (eqv? 0 num)
				(mset->list (cdr mset))
				(cons (1st (1st mset)) (loop (- num 1))))
			 ))))
	
(define make-multiset
	(lambda (lst)
		(let ([mset (list->mset lst '())])
			(lambda (msg . args)
				(case msg ; 
					[(syms) (map 1st mset)]
					[(add!) (begin(set! mset (change-mset mset (1st args)))(make-multiset (mset->list mset)))]
					[(rep) (list->mset lst '())]
					[(count)(if (null? args)
								(apply + (map 2nd mset))
								(count-sym mset (1st args)))]
					[(empty?)(null? mset)]
					[else (errorf 'make-multiset "illegal message to make-multiset object: ~a" msg)])))))
					
					
					