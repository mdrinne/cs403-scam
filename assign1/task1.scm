(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg (readExpr))
    (cym arg)
    )


(define (cym i)
    (println "(cyan " i ") is " (getCyan i))
    (println "(yellow " i ") is " (getYellow i))
    (println "(magenta " i ") is " (getMagenta i))

    (println "(cym " 
             i
             ") is #"
             (getHexStr (getCyan i))
             (getHexStr (getYellow i)) 
             (getHexStr (getMagenta i))
             )
    )


(define (getCyan i)
    (define pi 3.14159265358979323846)

    (int
        (+ 
            (*
                (sin 
                    (+
                        (*
                            (/ (real i) (real 100))
                            (/ pi 2)
                            )
                        (/ pi 2)
                        )
                    )
                255
                )
            0.00000001
            )
        )
    )


(define (getYellow i)
    (define pi 3.14159265358979323846)

    (int
        (+
            (*
                (+
                    (- 0
                        (sin
                            (*
                                (/ (real i) (real 100))
                                pi
                                )
                            )
                        )
                    1
                    )
                255
                )
            0.00000001
            )
        )
    )

(define (getMagenta i)
    (define pi 3.14159265358979323846)
    
    (int
        (+
            (*
                (/
                    (+
                        (sin
                            (+
                                (*
                                    (/ (real i) (real 100))
                                    (/
                                        (* 3 pi)
                                        2
                                        )
                                    )
                                (/ pi 2)
                                )
                            )
                        1
                        )
                    2
                    )
                255
                )
            0.00000001
            )
        )
    )


(define (getHexStr num)
    (string+ (getHexVal (/ num 16)) (getHexVal (% num 16)))
    )

(define (getHexVal num)
    (cond
        ((== num 15) "F")
        ((== num 14) "E")
        ((== num 13) "D")
        ((== num 12) "C")
        ((== num 11) "B")
        ((== num 10) "A")
        (else (string num))
        )
    )

