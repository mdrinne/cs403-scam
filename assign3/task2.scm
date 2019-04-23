(define (replace f items)
    (define (check temp pile)
        (if (null? (cddr pile))
            (if (eq? (car pile) temp)
                (cadr pile)
                temp
                )
            (if (eq? (car pile) temp)
                (cadr pile)
                (check temp (cddr pile))
                )
            )
        )


    (define (iter stuff)
        (cond
            ((null? stuff) stuff)
            ((object? (car stuff)) stuff)
            ((eq? (car stuff) 'quote) (set-car! stuff (check (car stuff) items)))
            ((eq? (type (car stuff)) 'CONS)
                (iter (car stuff))
                (iter (cdr stuff))
                )
            (else
                (set-car! stuff (check (car stuff) items))
                (iter (cdr stuff))
                )
            )
        )

    (iter (get 'parameters f))
    (iter (cadr (get 'code f)))
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )