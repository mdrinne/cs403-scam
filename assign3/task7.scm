(define (sdisplay n z)
    (define (iter x m)
        (cond
            ((= m 0) (print "..."))
            (else
                (print (stream-car x) ",")
                (iter (stream-cdr x) (- m 1))
                )
            )
        )

    (print "(")
    (iter z n)
    (print ")")
    )


(define (mystery x)
    (define (add-streams s t)
        (cons-stream
            (+ (stream-car s) (stream-car t))
            (add-streams (stream-cdr s) (stream-cdr t))
            )
        )


    (define (stream-exp s)
        (cons-stream (^ x (stream-car s)) (stream-exp (stream-cdr s)))
        )


    (define (stream-mult s t)
        (cons-stream (* (stream-car s) (stream-car t)) (stream-mult (stream-cdr s) (stream-cdr t)))
        )


    (define (even-fact s ind)
        (cons-stream (* (stream-car s) ind (+ ind 1.0)) (even-fact (stream-cdr s) (+ ind 2.0)))
        )


    (define (div-streams s t)
        (cons-stream (/ (real (stream-car s)) (stream-car t)) (div-streams (stream-cdr s) (stream-cdr t)))
        )


    (define alt-ones (cons-stream 1.0 (cons-stream -1.0 alt-ones)))
    (define twos (cons-stream 2.0 twos))
    (define evens (cons-stream 0.0 (add-streams twos evens)))
    (define top (cons-stream (^ x (stream-car evens)) (stream-exp (stream-cdr evens))))
    (define numer (cons-stream (* (stream-car alt-ones) (stream-car top)) (stream-mult (stream-cdr alt-ones) (stream-cdr top))))
    (define denom (cons-stream 1.0 (even-fact denom 1.0)))
    (define mys (cons-stream (/ (real (stream-car numer)) (stream-car denom)) (div-streams (stream-cdr numer) (stream-cdr denom))))
    )


(define (ps-mystery x)
    (define (accum-stream s acc)
        (cons-stream (+ (stream-car s) (stream-car acc)) (accum-stream (stream-cdr s) (stream-cdr acc)))
        )
    

    (define mys (mystery x))
    (define ps (cons-stream (stream-car mys) (accum-stream (stream-cdr mys) ps)))
    )


(define (euler-trans s)
    (define (stream-ref s n)
        (if (= n 0)
            (stream-car s)
            (stream-ref (stream-cdr s) (- n 1))
            )
        )


    (define (square x) (* x x))


    (let
        ((s0 (stream-ref s 0))
         (s1 (stream-ref s 1))
         (s2 (stream-ref s 2)))
        (cons-stream 
            (if (= (+ s0 (* -2 s1) s2) 0)
                s2 
                (- s2 (/ (square (- s2 s1))
                         (+ s0 (* -2 s1) s2)))
                )
            (euler-trans (stream-cdr s))
            )
        )
    )


(define (acc-mystery x)
    (euler-trans (ps-mystery x))
    )


(define (super-mystery x)
    (define (make-tableau trans s)
        (cons-stream s (make-tableau trans (trans s)))
        )


    (define (acc-seq trans s)
        (define (stream-map f s)
            (cons-stream
                (f (stream-car s))
                (stream-map f (stream-cdr s))
                )
            )


        (stream-map stream-car (make-tableau trans s))
        )
    

    (acc-seq euler-trans (ps-mystery x))
    )


(define (symbolic-mystery)
    (println "(mystery x) is $\\cos x$")
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )