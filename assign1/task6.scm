(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define f (eval (readExpr) this))
    (define arg2 (readExpr))

    (shanks f arg2)
    )

(define (shanks f i)
    (define w
        (lambda (x)
            (if (= x 0) (f x) (func))
            )
        )

    (define (func)
        (define base (s i))
        (define ip1 (s (+ i 1)))
        (define im1 (s (- i 1)))
        (/ (real (- (real (* ip1 im1))
              (real (* base base))
                ))
           (real (+ (- ip1 (* base 2))
                    im1
                    ))
            )
        )

    (define (sum n store count)
        (if (= count n) (+ store (f count))
            (sum n (+ store (f count)) (+ count 1))
            )
        )

    (define s
        (lambda (x)
            (sum x 0 0)
            )
        )
    
    (println
        "(S "
        s
        " "
        i
        ") is "
        (fmt "%.15f" (s i))
        )
    (println
        "(w "
        w
        " "
        i
        ") is "
        (fmt "%.15f" (w i))
        )
    )