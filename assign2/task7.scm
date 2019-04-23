(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
            )
        )
    )

(define (accumulate-n op initial sequence)
    (if (null? (car sequence))
        nil
        (cons
            (accumulate op initial (map car sequence))
            (accumulate-n op initial (map cdr sequence))
            )
        )
    )


(define (dot-product v w)
    (accumulate + 0 (map * v w))
    )


(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m)
    )


(define (matrix-*-matrix m n)
    (map (lambda (x)
            (map (lambda (y)
                    (dot-product x y))
                (transpose m)))
            n)
    )


(define (transpose mat)
    (accumulate-n cons nil mat)
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))

    ; (println (transpose '((1 4 7) (2 5 8) (3 6 9))))
    ; (println (transpose '((1 0) (0 1))))
    ; (println (matrix-*-matrix '((1 2) (3 4)) '((1 0) (0 1))))
    )