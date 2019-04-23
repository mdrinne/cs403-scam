(include "queue.scm")
(include "gates.scm")
(include "nand.scm")

(define the-agenda (make-agenda))


(define (inverter x y)
    (nand-gate x x y)
    )

(define (and-gate x y z)
    (let ((a (make-wire)))
        (nand-gate x y a)
        (inverter a z)
        )
    )

(define (or-gate x y z)
    (let ((a (make-wire)) (b (make-wire)))
        (nand-gate x x a)
        (nand-gate y y b)
        (nand-gate a b z)
        )
    )

(define (nor-gate x y z)
    (let ((a (make-wire)))
        (or-gate x y a)
        (inverter a z)
        )
    )


(define (xor-gate x y z)
    (let ((a (make-wire)) (b (make-wire)) (c (make-wire)))
        (nand-gate x y a)
        (nand-gate x a b)
        (nand-gate y a c)
        (nand-gate b c z)
        )
    )


(define (xnor-gate x y z)
    (let ((d (make-wire)))
        (xor-gate x y d)
        (inverter d z)
        )
    )


(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok
        )
    )


(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok
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