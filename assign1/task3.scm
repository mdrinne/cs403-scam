(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))

    ((root-n arg1) arg2)
    )

(define (root-n n)
    (lambda (x)
    (println 
        "((root-n " 
        n 
        ") " 
        x 
        ") is " 
        (fmt "%.15f" (n-root x n))
        ))
    )

(define (n-root x n)
    (n-root-iter 1.0 x n)
    )

(define (n-root-iter guess x n)
    (if (close-enough? guess (better guess x n))
        (better guess x n)
        (n-root-iter (better guess x n) x n)
        )
    )


(define (close-enough? guess bg)
    (= guess bg)
    )

(define (better guess x n)
    (/ (+ (* (- n 1) guess)
          (/ x
             (^ guess (- n 1))
             )
          )
        n
        )
    )
