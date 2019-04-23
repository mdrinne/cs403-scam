(define (let*->lambdas x)
    (define (let*? x)
        (if (null? x)
            #f
            (if (equal? (car x) 'let*)
                #t
                (let*? (cdr x))
                )
            )
        )


    (define (getLambdas vars body)
        (define (getBody body)
            (if (null? body)
                nil
                (cons (car body) (cdr body))
                )
            )

        (if (null? vars)
            (getBody body)
            (begin
                (define var (car vars))
                (list 
                    (cons 
                        'lambda
                        (cons
                            (list (car var)) 
                            (if (null? (cdr vars))
                                (getBody body)
                                (list (getLambdas (cdr vars) body))
                                )
                            )
                        ) 
                    (cadr var))
                )
            )
        )


    (if (let*? (caddr x))
        (list (car x) (cadr x) (getLambdas (cadr (caddr x)) (cddr (caddr x))))
        x
        )
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )