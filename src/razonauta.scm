#lang scheme

(require (lib "class.ss"))
 

(define Probador
    (class object%
        (field 
            (deduccion null)
            (_arbol null)
        )

        ;; -------------- METODOS PRINCIPALES -------------- ;;

        (define/public (acepte-deduccion pExpresion)
            (define d1 (new Deduccion))
            (set! d1 (car (parser-deduccion pExpresion)))

            (display (send d1 get-premisa))
            (display " => ")
            (display (send d1 get-conclusion))
            (newline) (display "Listo.")
        )

        (define/public (justifique)
            (cond
                ((null? deduccion)
                    (display "No hay deducción")
                )
                (else
                    (display "Hay deducción")
                )
            )
        )

        (define/public (responda)
            (display "mesta")
        )

        (define/public (prueba-deduccion)
            (display "esta")
        )

        (define/public (arbol)
            (display "arbol")
        )

        ;; -------------- OTROS METODOS -------------- ;;

        (define/public (parser-deduccion pExpresion)
            ; (p->q) => ((~q)->(~p)) - LO PASA A LISTA - "(p->q)" "=>" "((~q)->(~p))"
            (define listaExp (string-split pExpresion))
            (define tempDeduc (new Deduccion))

            (cond ((> (length listaExp) 0)
                    (send tempDeduc set-premisa (car listaExp))
                    (cond ((>= (length listaExp) 3)
                            (send tempDeduc set-conclusion (car (cdr (cdr listaExp))))
                        )
                    ) ;(else ("Expresión incompleta"))
                ) ;(else (display "No se ingresó una expresión"))
            
            )
            ; return
            (list tempDeduc)
        )

        ;; ----------------- ;;
        (super-new)
    )
)

(define Deduccion
    (class object%
        ;(init-field expresion)
        (field
            (premisa null) (conclusion null) ; ej. (p->q) => ((~q)->(~p))
            (formula null)
            (regla null)
        )

        (define/public (set-premisa pPremisa)
            (set! premisa pPremisa)            
        )

        (define/public (get-premisa) premisa)

        (define/public (set-conclusion pConclusion)
            (set! conclusion pConclusion)            
        )

        (define/public (get-conclusion) conclusion)

        (define/public (set-formula pFormula)
            (set! formula pFormula)
        )

        (define/public (set-regla pRegla)
            (set! regla pRegla)
        )

        (super-new)
    )
)


(define p1 (new Probador))
(send p1 acepte-deduccion "(p->q) => ((~q)->(~p)))")
