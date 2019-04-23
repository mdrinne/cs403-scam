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


(define (quad a b c step)
    (define (square x) (* x x))
    (define (get-f val) (+ (* a (square val)) (* b val) c))


    (define (stepper s)
        (cons-stream (+ (stream-car s) (real step)) (stepper (stream-cdr s)))
        )


    (define (f-stream s)
        (cons-stream (get-f (stream-car s)) (f-stream (stream-cdr s)))
        )


    (define x (cons-stream (real 0) (stepper x)))
    (define f (cons-stream (get-f (stream-car x)) (f-stream (stream-cdr x))))
    )


(define (integrate s h)
    (define (trap-area b1 b2)(* (/ (+ b1 b2) 2) h))


    (define (integ-help st self)
        (cons-stream (+ (stream-car self) (trap-area (stream-car st) (stream-car (stream-cdr st)))) (integ-help (stream-cdr st) (stream-cdr self)))
        )

    (define integ (cons-stream (real 0) (integ-help s integ)))
    )


(define (derivate s step f0)
    (define (undo integ b1)
        (- (* (/ integ step) 2) b1)
        )


    (define (deriv s step b1)
        (let ((trap (- (stream-car (stream-cdr s)) (stream-car s))))
            (define b2 (undo trap b1))
            (cons-stream b2 (deriv (stream-cdr s) step b2))
            )
        )


    (cons-stream (real f0) (deriv s step f0))
    )


(define (same-stream? s t count thresh)
    (cond
        ((= count 0) #t)
        ((<= (abs (- (stream-car s) (stream-car t))) thresh) (same-stream? (stream-cdr s) (stream-cdr t) (- count 1) thresh))
        (else #f)
        )
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )