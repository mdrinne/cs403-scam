(define (ssize st)
    (cdr st)
    )


(define (speek st)
    (car (car st))
    )


(define (push st val)
    (cons (cons val (car st)) (+ 1 (cdr st)))
    )


(define (pop st)
    (cons (cdr (car st)) (- 1 (cdr st)))
    )


(define (Stack)
    (cons (list) 0)
    )


(define (qsize qu)
    (cdr qu)
    )


(define (qpeek qu)
    (if (null? (cdr (car qu)))
        (car (reverse (car (car qu))))
        (car (cdr (car qu)))
        )
    )


(define (enqueue qu val)
    (cons 
        (cons 
            (cons val (car (car qu)))
            (cdr (car qu))
            ) 
        (+ 1 (cdr qu))
        )
    )


(define (reverse items)
	(define (iter store src)
		(cond
			((null? src) store)
			(else (iter (cons (car src) store) (cdr src)))
			)
		)
	(iter nil items)
	)


(define (dequeue qu)
    (if (null? (cdr (car qu)))
        (cons
            (cons
                (list)
                (cdr (reverse (car (car qu))))
                )
            (- 1 (cdr qu))
            )
        (cons
            (cons
                (car (car qu))
                (cdr (cdr (car qu)))
                )
            (- 1 (cdr qu))
            )
        )
    )


(define (Queue)
    (cons (cons (list) (list)) 0)
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )