(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )


(define (range start end step)
    (if (>= start end) nil
        (cons start (range (+ start step) end step))
        )
    )


(define (for-loop nums lamb)
    (define (exec len count nums lamb)
        (if (< count len)
            (begin
                (lamb (getElement nums count))
                (exec len (+ count 1) nums lamb)
                )
            )
        )
        
    (exec (length nums) 0 nums lamb)
    )

