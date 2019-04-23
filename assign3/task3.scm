(define (node v pa ch)
    (define val v)
    (define parent pa)
    (define child ch)

    (define (set-parent p)
        (set 'parent p)
        )


    (define (set-child c)
        (set 'child c)
        )


    (define (set-val va)
        (set 'val va)
        )

    
    (define (get-parent)
        parent
        )

    
    (define (get-child)
        child
        )


    (define (get-val)
        val
        )


    this
    )


(define (deque)
    (define stuff nil)
    (define size 0)
    (define tail nil)


    (define (enqueueFront val)
        (define n (node val nil nil))
        (if (null? stuff)
            (begin
                (set 'size 1)
                (set 'stuff n)
                (set 'tail n)
                )
            (begin
                ((n'set-child) stuff)
                ((stuff'set-parent) n)
                (set 'stuff n)
                (++ size)
                )
            )
        val
        )


    (define (enqueueBack val)
        (define n (node val nil nil))
        (if (null? stuff)
            (begin
                (set 'size 1)
                (set 'stuff n)
                (set 'tail n)
                )
            (begin
                ((n'set-parent) tail)
                ((tail'set-child) n)
                (set 'tail n)
                (++ size)
                )
            )
        val
        )


    (define (enqueueIndex ind val)
        (define (e-iter src count in va)
            (if (= count in)
                (begin
                    (define c ((src'get-child)))
                    (define n (node va src c))
                    ((src'set-child) n)
                    (if (= in size)
                        (set 'tail n)
                        ((c'set-parent) n)
                        )
                    )
                (e-iter ((src'get-child)) (+ count 1) in va)
                )
            )

        (if (= ind 0)
            (enqueueFront val)
            (begin
                (e-iter stuff 1 ind val)
                (++ size)
                )
            )
        val
        )


    (define (dequeueFront)
        (define v ((stuff'get-val)))
        (set 'stuff ((stuff'get-child)))
        (-- size)
        (if (= size 0)
            (set 'tail nil)
            ((stuff'set-parent) nil)
            )
        v
        )


    (define (dequeueBack)
        (define v ((tail'get-val)))
        (set 'tail ((tail'get-parent)))
        (-- size)
        (if (= size 0)
            (set 'stuff nil)
            ((tail'set-child) nil)
            )
        v
        )


    (define (dequeueIndex ind)
        (define (d-iter src ind count)
            (if (= ind count)
                (begin
                    (define c ((src'get-child)))
                    (define p ((src'get-parent)))
                    (define v ((src'get-val)))
                    ((c'set-parent) p)
                    (if (!= count 0)
                        ((p'set-child) c)
                        (set 'stuff c)
                        )
                    v
                    )
                (d-iter ((src'get-parent)) ind (- count 1))
                )
            )
        

        (if (= ind (- size 1))
            (dequeueBack)
            (begin
                (define v (d-iter ((tail'get-parent)) ind (- size 2)))
                (-- size)
                v
                )
            )
        )

    
    (define (display)
        (define (d-iter src)
            (if (not (null? src))
                (begin 
                    (print ((src'get-val)))
                    (if (not (null? ((src'get-child))))
                        (begin 
                            (print ",")
                            (d-iter ((src'get-child)))
                            )
                        )
                    )
                )
            )

        (print "[")
        (d-iter stuff)
        (print "]")
        )


    (define (peekFront)
        ((stuff'get-val))
        )

    
    (define (peekBack)
        ((tail'get-val))
        )
        

    this
    )


(define (main)
    (define env this)
    (define (iter expr)
         (if (not (eof?)) (begin (eval expr env) (iter (readExpr))))
         )
    (setPort (open (getElement ScamArgs 1) 'read))
    (iter (readExpr))
    )