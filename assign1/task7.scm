(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))

    (ethiop arg1 arg2)
    )


(define (ethiop a b)
    (println "(halve " a ") is " (halve a)
        )
    (println "(double " a ") is " (double a)
        )
    (println "(div2? " a ") is " (div2? a)
        )
    (println "(ethiop " a " " b ") is " (ethiop-iter a b 0)
        )
    )


(define (ethiop-iter a b store)
    (cond
        ((= b 1) (+ store a))
        ((and (div2? b) #t) (ethiop-iter (double a) (halve b) store))
        (else (ethiop-iter (double a) (halve b) (+ a store)))
        )
    )


(define (double x)
    (+ x x)
    )


(define (halve x)
    (define pow (get-power x 1 0))
    (define temp (get-2-pow pow 0 1))
    (iter x temp pow 0 0)
    )

(define (iter x cur count store ans)
    (if (>= 0 count) ans
        (begin
            (define check (get-2-pow count 0 1))
            (define next (get-2-pow (- count 1) 0 1))
            (if (>= x (+ store check))
                (iter x next (- count 1) (+ store check) (+ ans next))
                (iter x next (- count 1) store ans)
                )
            )
        )
    )

(define (get-2-pow x count store)
    (if (= count x)
        store
        (get-2-pow x (+ 1 count) (double store))
        )
    )


(define (get-power x num pow)
    (if (<= (double num) x)
            (get-power x (double num) (+ pow 1))
            pow
        )
    )


(define (div2? x)
    (if (= x (double (halve x))) #t #f)
    )