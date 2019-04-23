(define (sdisplay n z)
    (define (iter x m)
        (cond
            ((= m 0) (print "..."))
            (else
                (print (stream-car x) ",")
                (iter (stream-cdr x) (- m 1))
                )
            )
        )

    (print "(")
    (iter z n)
    (print ")")
    )


(define (add-streams s t)
    (cons-stream
        (+ (stream-car s) (stream-car t))
        (add-streams (stream-cdr s) (stream-cdr t))
        )
    )


(define (smush s op)
    (define (trip a b c z)
        (cond
            ((= z 0) (cons-stream (stream-car a) (trip (stream-cdr a) b c 1)))
            ((= z 1) (cons-stream (stream-car b) (trip a (stream-cdr b) c 2)))
            ((= z 2) (cons-stream (stream-car c) (trip a b (stream-cdr c) 0)))
            )
        )


    (define (accum-stream s in)
        (define cur (op in (stream-car s)))
        (cons-stream cur (accum-stream (stream-cdr s) cur))
        )

    (define ones (cons-stream 1 ones))
    (define wholes (cons-stream 0 (add-streams ones wholes)))
    (define temp (cons-stream (stream-car s) (accum-stream (stream-cdr s) (stream-car s))))

    (cons-stream (stream-car wholes) (trip (stream-cdr wholes) s temp 1))
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )