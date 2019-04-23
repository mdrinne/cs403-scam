(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define depth (readExpr))

    (ramanujanr depth)
    )


(define (ramanujanr depth)
    (println
        "(ramanujanr "
        depth
        ") is "
        (fmt "%.25f" (iter 1 6 0 depth))
        )
    (println "$4$")
    )


(define (iter mult base count depth)
    (if (= depth count) (* mult (sqrt base))
        (* mult (sqrt (+ base (iter (+ mult 1) (+ base 1) (+ count 1) depth))))
        )
    )


; (define (sqrt x)
;     (sqrt-iter 1.0 x)
;     )


; (define (sqrt-iter guess x)
;     (if (close-enough? guess (better guess x)) (better guess x)
;         (sqrt-iter (better guess x) x)
;         )
;     )


; (define (close-enough? guess bg)
;     (= guess bg)
;     )


; (define (better guess x)
;     (avg guess (/ x guess))
;     )


; (define (avg x y)
;     (/ (+ x y) 2)
;     )