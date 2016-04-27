;Hua Yang AS04 3.15.2015

;Problem 1
(define isListOfList
    (lambda (list)
      (if (null? list)
          #t
          (if (not (list? (car list)))
              #f
              (isListOfList (cdr list))))))

(define isDuplicate
    (lambda (element list)
      (if (null? list)
          0
          (if (equal? element (car (car list)))
              (+ 1 (isDuplicate element (cdr list)))
              (isDuplicate element (cdr list))))))

(define multi-set-helper
    (lambda (list n length)
      (if (= length n)
           #t
           (if (and (= 1 (isDuplicate (car (list-ref list n)) list))
                    (and (not (integer? (car (list-ref list n))))
                         (and (integer? (cadr (list-ref list n)))
                              (> (cadr (list-ref list n)) 0))))
               (multi-set-helper list (+ 1 n) length)
               #f))))

(define multi-set?
    (lambda (list)
      (if (not (isListOfList list))
          #f
          (multi-set-helper list 0 (length list)))))

;Problem 2
(define ms-size
    (lambda (l)
      (apply + (map (lambda(x) (cadr x)) l))))

;Problem 3
(define matrix-ref
    (lambda (m i j)
      (list-ref (list-ref m i) j)))

;Problem 4
(define isSameLength?
    (lambda (m)
      (cond [(and (= 1 (length m)) (not (null? (car m)))) #t]
            [(and (> (length m) 1)
                  (and (and (not (null? (car m))) (not(null? (car (cdr m)))))
                       (= (length (car m)) (length (car (cdr m))))))
             (isSameLength? (cdr m))]
            [else #f])))

(define matrix?
    (lambda (m)
      (if (not (list? m))
          #f
          (isSameLength? m))))

;Problem 5
(define matrix-transpose-helper
    (lambda (m index)
      (if (= index (length (car m)))
          (list)
          (cons (map (lambda (x) (list-ref x index)) m) 
            (matrix-transpose-helper m (+ index 1)))))

(define matrix-transpose
    (lambda (m)
      (matrix-transpose-helper m 0)))

;Problem 6
(define last
    (lambda (list)
      (if (= 1 (length list))
          (car list)
          (last (cdr list)))))

;Problem 7
(define all-but-last
    (lambda (l)
      (if (= (length l) 1)
          (list)
          (cons (car l) (all-but-last (cdr l))))))