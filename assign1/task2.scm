(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define thresh (readExpr))
    (define x (readExpr))
    (define y (readExpr))

    ((mandelbrot thresh) x y)
    )

(define (mandelbrot thresh)
    (lambda (x y) 
        (println
            "((mandelbrot "
            thresh
            ") "
            x
            " "
            y
            ") is "
            (iter 0.0 0.0 x y thresh 0)
            )
        )
    )


(define (iter r s x y thresh count)
    (if (= count thresh) 0
        (if (= (test-div r s) 1) count
            (iter (+ .0001 (get-r r s x)) (+ .0001 (get-s r s y)) x y thresh (+ count 1))
            )
        )
    )


(define (test-div r s)
    (if (> (+ (sqr r) (sqr s)) 4) 1 0)
    )


(define (get-r r s x)
    (+ (- (sqr r) (sqr s))
        x
        )
    )


(define (get-s r s y)
    (+ (* 2 r s)
        y
        )
    )


(define (sqr x)
    (* x x)
    )