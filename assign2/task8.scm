(define (node value left right)
        (define (display) (print value))
        this
        )


(define (newBST value)
    (node value nil nil)
    )


(define (displayBST root)
    (define (iter root indent)
        (if (valid? root)
            (begin
                (iter (root'right) (string+ indent "    "))
                (print indent)
                ((root'display))
                (println)
                (iter (root'left) (string+ indent "    "))
                )
            )
        )
    (iter root "")
    )


(define (insertBST tree val)
    (define (l-child? n)
        (if (null? (n'left))
            #f
            #t
            )
        )


    (define (r-child? n)
        (if (null? (n'right))
            #f
            #t
            )
        )

    (if (< val (tree'value))
        (if (l-child? tree)
            (node (tree'value) (insertBST (tree'left) val) (tree'right))
            (node (tree'value) (node val nil nil) (tree'right))
            )
        (if (r-child? tree)
            (node (tree'value) (tree'left) (insertBST (tree'right) val))
            (node (tree'value) (tree'left) (node val nil nil))
            )
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
