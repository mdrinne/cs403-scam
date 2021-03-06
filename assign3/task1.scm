(define (scoping sym obj)
    (define (iter sym env lvl)
        (cond
            ((local? sym env) (if (= lvl 0) 'bound 'free))
            ((null? (dot env __context)) 'undefined)
            (else (iter sym (dot env __context) (+ lvl 1)))
            )
        )

    (iter sym obj 0)
    )

(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )