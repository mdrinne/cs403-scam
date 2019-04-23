(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define depth (readExpr))

    (ecfr depth)
    )


(define (ecfr depth)
    (println
        "(ecfr "
        depth
        ") is "
        (fmt "%.25f" (real (+ 2 (if (= depth 0) 0 (/ 1 (real (recur 1 (* depth 3) 1)))))))
        )
    )


(define (recur counter depth mult)
    (cond
        ((= (% counter 3) 2) (+ (* 2 mult) (/ 1 (real (recur (+ counter 1) depth (+ mult 1))))))
        ((= counter depth ) 1)
        (else (+ 1 (/ 1 (real (recur (+ counter 1) depth mult)))))    
        )
    )