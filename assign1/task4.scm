(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))
    (define arg3 (readExpr))
    ((crazy-triangle arg1 arg2) arg3)
    )

(define (crazy-triangle l r)
    (lambda (depth)
        (println
            "((crazyTriangle "
            l
            " "
            r
            ") "
            depth
            ")"
            )
        (get-triangle l r depth 1)
        )
    )

(define (get-triangle l r depth row)
    (if (< row depth) (get-spaces row depth))
    (get-row l r row 1)
    (println)
    (if (!= depth row) (get-triangle l r depth (+ row 1)))
    )

(define (get-spaces count depth)
    (print " ")
    (if (< count (- depth 1)) (get-spaces (+ count 1) depth))
    )

(define (get-row l r row col)
    (if (<= col row) (print (get-val l r row col)))
    (if (< col row) (print " "))
    (if (< col row) (get-row l r row (+ col 1)))
    )

(define (get-val l r row col)
    (cond ((= col 1) l)
        ((= col row) r)
        (else (+ (get-val l r (- row 1) (- col 1))
                 (get-val l r (- row 1) col)))
        )
    )