(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define depth (readExpr))

    (ramanujani depth)
    )


(define (ramanujani depth)
    (println
        "(ramanujani "
        depth
        ") is "
        (fmt "%.25f" (iter (+ 1 depth) (+ 6 depth) depth 0))
        )
    (println "$4$")
    )


(define (iter mult base count store)
    (if (= count 0) (sqrt (+ 6 store))
        (iter (- mult 1) (- base 1) (- count 1) (* mult (sqrt (+ base store))))
        )
    )