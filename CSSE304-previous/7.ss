;Bo Peng Assignment 7 

;Problem 1
(define lst-append(lambda (nv lst offset index)
	(if (null? lst) 
		nv 
		(and (vector-set! nv (+ index offset) (car lst )) (lst-append nv (cdr lst) offset (+ 1 index))))))
	
(define vec-append (lambda (nv vec index)
	(if (< index (vector-length vec))
		(and (vector-set! nv index (vector-ref vec index )) (vec-append nv vec (+ 1 index)))
		nv)))
			
(define vector-append-list (lambda (v lst)
	(lst-append 
		(vec-append 
			(make-vector (+ (vector-length v)(length lst))) 
			v 
			'0 ) 
		lst 
		(vector-length v) 
		'0)
	)
)

;Problem 2
(define get-left (lambda (ls pt)
	(if 
		(or (< pt 1) (> pt (length ls)) ) 
		'() 
		(if 
			(< 0 (- pt 1)) 
			(get-left (cdr ls) (- pt 1)) 
			(car ls) ) )))
		
(define get-right (lambda (ls pt)
	(if 
		(or (< pt -1) (>= (+ pt 1) (length ls)) ) 
		'() 
		(if 
			(< 0 (+ pt 1)) 
			(get-right (cdr ls) (- pt 1)) 
			(car ls) ) )))
			
(define get-ele (lambda (ls pt)
	(if 
		(or (< pt -1) (> (+ pt 1) (length ls)) ) 
		'() 
		(if 
			(< 0 pt) 
			(get-ele (cdr ls) (- pt 1)) 
			(car ls) ) )))

(define split-left (lambda (pivot ls result)
	(if (< (length result) pivot) 
		(split-left pivot (cdr ls) (cons  (car ls) result))
		(reverse result))))
		
(define split-right (lambda (pivot ls)
	(if (eqv? -1 pivot) ls (if (eqv? pivot (length ls)) 
		'() 
		(if (eqv? (+ pivot 1) 0)
		ls 
		(split-right (- pivot 1) (cdr ls)))))))

	
(define remove-from-list (lambda (ls index)
	(append (split-left index ls '()) (split-right index ls))))
	
(define condition-not-fit (lambda (ls pt pt2)
	  (append 
	  (split-left pt (remove-from-list ls pt2) '()) 
	  (list (get-ele ls pt2)) 
	  (split-right (- pt 1) (remove-from-list ls pt2)) )))

	  
(define left-branch (lambda (pred pt ls)
	(if (null? ls) 
		'()
		(if (pred (car ls) pt) 
			(cons (car ls)(left-branch pred pt (cdr ls)))
			(left-branch pred pt (cdr ls))))))
(define right-branch (lambda (pred pt ls)
		(if (null? ls) 
		'()
		(if (pred (car ls) pt)
			(right-branch pred pt (cdr ls))
			(cons (car ls)(right-branch pred pt (cdr ls)))
			))))

(define correct? (lambda (pred ls pt)
	(if (eqv? (+ pt 1) (length ls))
		#t
		(and (pred (get-ele ls pt) (get-ele ls (+ pt 1)))(correct? pred ls (+ pt 1))  ))))

(define qsort (lambda (pred ls)
	(if (null? ls) 
		ls
		(append (qsort pred (left-branch pred (car ls) (cdr ls)))
			(list(car ls))
			(qsort pred (qsort pred (right-branch pred (car ls) (cdr ls))))))))
		
;Problem 3
(define contain? (lambda (e l)
	(if (null? l) #f (if (eqv? e (car l)) #t (or #f (contain? e (cdr l)))))))
	
(define intersect (lambda (l1 l2)
	(if 
		(null? l2) 
		'() 
		(if (list? l2)	
			(if (contain? (car l2) l1) (append (list (car l2)) (intersect l1 (cdr l2))) (intersect l1 (cdr l2)))
			(if (contain? (car (list l2)) l1) (append (list (car (list l2))) (intersect l1 (cdr (list l2)))) (intersect l1 (cdr (list l2))))))))

			
(define list-tran (lambda (ls)
	(append (list (car ls)) (cadr ls))))
	
(define union (lambda (l1 l2)
	(if (or (null? l1)(null? l2)) 
		(append l1 l2) 
		(if (contain? (car l2) l1) 
			(union l1 (cdr l2))
			(union (append l1 (list (car l2))) (cdr l2))) )))
	
(define combine-same (lambda (originalR test)
	(if (null? originalR) 
		(list test) 
		(if (null? (intersect (car originalR) test)) 
			(append (list (car originalR)) 
					(combine-same (cdr originalR) test)) 
			(append (list (union (car originalR) test)) 
					(combine-same (cdr originalR) test))))
	))
	
(define connected-helper (lambda (g ls)
	(if (null? g) 
		ls   
		(connected-helper (cdr g)(combine-same ls (list-tran (car g)))))))
		
(define connected? (lambda (g)
	(if (or (equal? 1 (length g))(equal? 0 (length g))) 
		#t 
		(if (equal? 1 (length (loop(connected-helper g '())))) #t #f ))))
		
(define loop (lambda (ls)
	(if  (equal? (length(combine(combine ls))) (length(combine ls))) 
	(combine ls) 
	(loop (combine ls)))
))
		
(define combine (lambda (ls)
	(if (equal? (length ls) 1) 
		ls 
		(if  (not(equal? (length ls) 1))  
			(if (not (null?(intersect (car ls) (cadr ls)))) 
				(combine (append (list (union (car ls) (cadr ls))) (cddr ls)) ) 
				(append (combine (cons (cadr ls) (cddr ls))) (list(car ls)) )
		))
	) 
 ))
		

	
;Problem 4
(define reverse-it (lambda (lst)
	(let list-loop ([sub-list lst] [result '()])
		(if (null? sub-list)
			result
			(list-loop (cdr sub-list) (append (list (car sub-list) ) result) )))))
		
;Program 5
(define get-right-tree(lambda (ls)
	(if (list? ls) (caddr ls) '())))

(define get-left-tree (lambda (ls)
	(if (list? ls) (cadr ls) '())))

(define get-top-node (lambda (ls)
	(if (list? ls) (car ls) '())))
	
(define empty-BST (lambda () 
	'()))
	
(define empty-BST? (lambda (obj)
	(equal? (empty-BST) obj) ))

(define BST-iter (lambda (num bst)
	(cond 
	[(> (get-top-node bst) num) 
		(if (null? (get-left-tree bst))
			(list (get-top-node bst) (list num '() '()) (get-right-tree bst))
			(list (get-top-node bst) (BST-iter num (get-left-tree bst)) (get-right-tree bst)) )]
	[(< (get-top-node bst) num)
		(if (null? (get-right-tree bst))
			(list (get-top-node bst) (get-left-tree bst) (list num '() '()) )
			(list (get-top-node bst) (get-left-tree bst) (BST-iter num (get-right-tree bst))) )]
	[else bst]
	)))
	
(define BST-right (lambda (bst)
	(get-right-tree bst)))
	
(define BST-left (lambda (bst)
	(get-left-tree bst)))
	
(define BST-element (lambda (bst)
	(get-top-node bst)))
	
(define BST-insert (lambda (num bst)
	(if (empty-BST? bst) (list num '() '()) 
		 (BST-iter num bst))))

(define BST-inorder(lambda (bst)
   (if (null? bst) 
		bst
       (append 
			(BST-inorder (get-left-tree bst)) 
			(list (get-top-node bst)) 
			(BST-inorder (get-right-tree bst))))))

(define sort? 
	(lambda (ls l r)
		(cond 
			[(null? ls) #t]
			[(and (null? (cadr ls))(null? (caddr ls))) (and (> (car ls) l)(< (car ls) r)) ]
			[(and (null? (cadr ls))(not (null? (caddr ls)))) 
				(if (and (> (car ls) l)(< (car ls) r)) (sort? (caddr ls) (car ls) r) #f)]
			[(and (not (null? (cadr ls)))(null? (caddr ls))) 
				(if (and (> (car ls) l)(< (car ls) r)) (sort? (cadr ls) l (car ls)) #f)]
			[else (if (and (> (car ls) l)(< (car ls) r))
				(and (sort? (cadr ls) l (car ls)) (sort? (caddr ls) (car ls) r)) #f)]
			)))

(define check-BST? 
	(lambda (bst)
		(if (list? bst) 
			(cond 
				[(null? bst) #t]
				[(and 
						(eq? (length bst) 3)
						(list? (get-left-tree bst))
						(number? (get-top-node bst)) 
						(list? (get-right-tree bst))
						(and (check-BST? (get-left-tree bst)) 
							(check-BST? (get-right-tree bst)))) #t] 
			[else #f]) #f)))
		
(define BST?
    (lambda (bst)
      (if (check-BST? bst) (sort? bst -10000 10000) #f)))

(define BST-insert-nodes(lambda (bst nums)
      (if (null? nums) bst
          (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?(lambda (bst num)
      (cond [(null? bst) #f]
			[(> num (get-top-node bst)) (BST-contains? (get-right-tree bst) num)]
			[(< num (get-top-node bst)) (BST-contains? (get-left-tree bst) num)]
            [else #t]
            )))
			
(define sum-until-0 (lambda (num)(letrec ([sum (lambda (x)
             (if (zero? x)
                 0
                 (+ x (sum (- x 1)))))])
   (sum num))))
   
;Program 6
(define map-by-position (lambda (fn-list arg-list)
	(map (lambda (fn arg) (fn arg)) fn-list arg-list )))

;Problem 7
(define bintree? (lambda (T)
	(cond [(null? T) #t]
		  [(integer? T) #t]
		  [else (if (eqv? (length T) 3) 
						 (and (symbol? (car T)) (bintree? (cadr T)) (bintree? (caddr T)))  
						 #f)]
		  )))

(define bt-leaf-sum (lambda (T)
	(cond [(not (bintree? T)) #f]
		  [(number? T) T]
		  [(list? T) (+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T)))]
		  [else display T])))	

	
(define bt-inorder-list (lambda (T)
	(if (or(null? T) (number? T))
		'()
		(append (bt-inorder-list (cadr T)) (list(car T))  (bt-inorder-list (caddr T)) ))))
		
(define bt-max (lambda (T)
	(cond 
		[(number? T) T]
		[(list? T) (max (bt-max (cadr T)) (bt-max (caddr T)))]
		[else false]
		)))

(define bt-max-interior (lambda (T)
	(car (cadr (bt-max-helper T)))))		
	
(define bt-max-helper 
	(lambda (T) 
		(cond 
			[(and (list? (cadr T)) (list? (caddr T)))
				(bt-max-filter (car T) (bt-max-helper (cadr T)) (bt-max-helper (caddr T)))]
			[(and (list? (cadr T))(number? (caddr T)))(bt-max-filter-r (car T) (caddr T) (bt-max-helper (cadr T)))]
			[(and (number? (cadr T)) (list? (caddr T)))(bt-max-filter-l (car T) (cadr T) (bt-max-helper (caddr T)))]
			[else (list 
						(+ (cadr T) (caddr T))
						(list (car T) (+ (cadr T) (caddr T))))]
			)))
			
(define bt-max-filter
	(lambda (midname llst rlst)
		(cond 
			[(> (cadr (cadr llst)) (cadr (cadr rlst))) 
				(if (> (+ (car llst) (car rlst)) (cadr (cadr llst))) 
					(list (+ (car llst) (car rlst)) (list midname (+ (car llst) (car rlst)))) 
					(list (+ (car llst) (car rlst)) (cadr llst)))]
			[(< (cadr (cadr llst)) (cadr (cadr rlst)))
				(if (> (+ (car llst) (car rlst)) (cadr (cadr rlst))) 
					(list (+ (car llst) (car rlst)) (list midname (+ (car llst) (car rlst)))) 
					(list (+ (car llst) (car rlst)) (cadr rlst)))]
			[else 
				(if (> (+ (car llst) (car rlst)) (cadr (cadr rlst)))
					(list (+ (car llst) (car rlst)) (list midname (+ (car llst) (car rlst))))
					(list (+ (car llst) (car rlst)) (cadr llst)))]
			)))
		
(define bt-max-filter-l
	(lambda (midname num lst)
		(cond 
			[(< num 0) (list (+ (car lst) num) (cadr lst))]
			[(> (+ (car lst) num) (cadr (cadr lst))) (list (+ (car lst) num) (list midname (+ (car lst) num)))]
			[else (list (+ (car lst) num) (list midname (+ (car lst) num)))]
			)))

(define bt-max-filter-r
	(lambda (midname num lst)
		(cond 
			[(< num 0) (list (+ (car lst) num) (cadr lst))]
			[(> (+ (car lst) num) (cadr (cadr lst))) (list (+ (car lst) num) (list midname (+ (car lst) num)))]
			[else (list (+ (car lst) num) (cadr lst))]
			)))		
		
		