(define (mergesort list1 list2)
    (cond
        ((null? list1) list2)
        ((null? list2) list1)
        ((<= (length (car list2)) (length (car list1))) (cons (car list2) (mergesort list1 (cdr list2))))
        (else (cons (car list1) (mergesort (cdr list1) list2)))
        )
    )


(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
        )
    )


(define (powerSet s)
    (if (null? s)
      (list nil)
      (let ((rest (powerSet (cdr s))))
            (mergesort rest 
                (map 
                    (lambda (x) (append (list (car s)) x)) 
                    rest
                    )
                )
            )
        )
    
    )


(define (main)
    (setNilDisplay 'nil)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )