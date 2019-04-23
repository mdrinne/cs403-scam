(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )


(define (iter src func a)
    (if (= src 0)
        a
        (iter (- src 1) func (func a))
        )
    )


(define create
    (lambda (n)
        (lambda (f)
            (lambda (a)
                (iter n f a)
                )
            )
        )
    )


(define pred
    (lambda (n)
        (lambda (f)
            (lambda (a)
                (define (piter src func a prev)
                    (if (equal? src a)
                        prev
                        (piter src func (func a) a)
                        )
                    )

                (piter ((n f) a) f a 0)
                )
            )
        )
    )