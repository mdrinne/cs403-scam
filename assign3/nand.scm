(define (nand-gate a1 a2 output)
    (define (logical-nand a b) (if (= (+ a b) 2) 0 1))

    (define (nand-action-procedure)
        (let ((new-value
                (logical-nand (get-signal a1) (get-signal a2))))
            (after-delay 6
                (lambda () (set-signal! output new-value))
                )
            )
        )
    (add-action! a1 nand-action-procedure)
    (add-action! a2 nand-action-procedure)
    'ok
    )