(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define depth (readExpr))

    (ecfi depth)
    )

(define (ecfi depth)
    (println
        "(ecfi "
        depth
        ") is "
        (fmt "%.25f" (real (+ (real 2) (if (= depth 0) 0 (iter (* depth 3) depth 1 depth)))))
        )
    )

(define (iter counter mult store depth)
    (cond
        ((= (* depth 3) counter) (iter (- counter 1) mult store depth))
        ((= counter 0) (real (/ 1 store)))
        ((= (% counter 3) 2) (iter (- counter 1) (- mult 1) (+ (real (* 2 mult)) (/ 1 (real store))) depth))
        (else (iter (- counter 1) mult (+ (real 1) (/ 1 (real store))) depth))
        )
    )