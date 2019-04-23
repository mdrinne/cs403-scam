(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (eval (readExpr) this))
    ;(define (f a b c d) (+ a b c d))
    (define a (readExpr))
    (define b (readExpr))
    (define c (readExpr))
    (define d (readExpr))

    (((((curry arg1) a) b) c) d)
    )


(define (curry f)
    (lambda (a)
        (lambda (b)
            (lambda (c)
                (lambda (d)
                    (println
                        "(((((curry "
                        f
                        ") "
                        a
                        ") "
                        b
                        ") "
                        c
                        ") "
                        d
                        ") is "
                        (f a b c d)
                        )
                    )
                )
            )
        )
    )