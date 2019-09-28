#lang scheme

; ⮕ ⟶ ⟷ 

(require (lib "class.ss"))
 

(define Probador
    (class object%
        (field 
            (deduccion null)
            (_arbol null)
            (baseConocimiento null)
        )

        ;; -------------- METODOS PRINCIPALES -------------- ;;

        (define/public (acepte-deduccion pExpresion)

            (init-baseConocimiento)

            (define p1 (new Parser))
            (set! deduccion (car (send p1 get-deduccion-root pExpresion)))

            (display (send deduccion get-premisa))
            (display " => ")
            (display (send deduccion get-conclusion))
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
            
            ; llamar a MotorPrueba
            (define m1 (new MotorPrueba))
            (send m1 probar-deduccion deduccion)
        )

        (define/public (arbol)
            (display "arbol")
        )

        ;; -------------- OTROS MÉTODOS -------------- ;;
        (define/public (init-baseConocimiento)
            (cond
                ((null? baseConocimiento)
                    (set! baseConocimiento (new BaseConocimiento))
                    (send baseConocimiento init)
                ) ; else ya existe la base de conocimiento
            )
            ;(display (send (car (send baseConocimiento get-listaInferencias)) get-premisas))
            ;(display (send (car (send baseConocimiento get-listaTautologias)) get-axioma-1))
        )

        ;; ----------------- ;;
        (super-new)
    )
)


(define Parser
    (class object%
    
        ; Dar formato a la entrada del usuario, al momento de hacer acepte-deduccion
        (define/public (get-deduccion-root pExpresion)
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

        (define/public (es-caracter-alfabetico? x)
            (and (char? x) (char-alphabetic? x)) ; return true o false
        )

        (define/public (convertirALista str)
            (list (string->list str))
        )

        (super-new)
    )
)


(define MotorPrueba
    (class object%
        
        (define/public (probar-deduccion pDeduccion)
            ; separa las premisas/conclusiones mediante la coma y las mete en una lista 
            (define lstPremisas (str-split (send pDeduccion get-premisa) #\,))
            (define lstConclusiones (str-split (send pDeduccion get-conclusion) #\,))
            (display lstConclusiones)
        )

        (define/public (str-split str ch) ; fuente: https://gist.github.com/matthewp/2324447
            (let ((len (string-length str)))
                (letrec
                ((split
                    (lambda (a b)
                    (cond
                        ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                        ((char=? ch (string-ref str b)) (if (= a b)
                            (split (+ 1 a) (+ 1 b))
                            (cons (substring str a b) (split b b))))
                            (else (split a (+ 1 b)))))))
                            (split 0 0))))

        (super-new)
    )
)


; Fuerza Bruta
(define Estratega
    (class object%
        (define/public (and-izquierdo argumento)
            (define listaArgumento (string->list argumento))
            (define operando_1 (car listaArgumento))
            (define operando_2 (caddr listaArgumento))
            (list operando_1 operando_2)
        )

        (super-new)
    )
)


(define ArbolPrueba
    (class object%
        
        ; Usar Estratega

        (super-new)
    )
)


(define Justificador
    (class object%
        
        

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


(define Inferencia
    (class object%
        (init-field nombre listaPremisas conclusion)

        (define/public (add-Premisa premisa)
            (set! listaPremisas (append listaPremisas premisa))
        )
      
        (define/public (set-premisas pListaPremisas)
            (set! listaPremisas pListaPremisas)            
        )
      
        (define/public (get-premisas) listaPremisas)

        (define/public (set-conclusion pConclusion)
            (set! conclusion pConclusion)            
        )

        (define/public (get-conclusion) conclusion)

        (define/public (set-nombre pNombre)
            (set! nombre pNombre)
        )

        (define/public (get-nombre) nombre)

        (super-new)
    )
)


(define Tautologia
    (class object%
        (init-field nombre axioma_1 axioma_2)

        (define/public (set-axioma-1 axioma)
            (set! axioma_1 axioma)            
        )
      
        (define/public (get-axioma-1)
             axioma_1)

        (define/public (set-axioma-2 axioma)
            (set! axioma_2 axioma)            
        )

        (define/public (get-axioma-2)
             axioma_2)

        (define/public (set-nombre pNombre)
            (set! nombre pNombre)
        )

        (define/public (get-nombre)
             nombre)

        (super-new)
    )
)


(define BaseConocimiento
    (class object%
        (field 
            (listaInferencias '())
            (listaTautologias '())
        )
    
        (define/public (init)
            (set-inferencias)
            (set-tautologias)
        )

        (define/public (get-listaInferencias) listaInferencias)

        (define/public (get-listaTautologias) listaTautologias)

        (define/public (set-inferencias)
            (define inferencia
                (new Inferencia
                    (nombre "Simplificación")
                    (listaPremisas '("p^q"))
                    (conclusion "p,q"))
            )
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Adjunción")
                (listaPremisas '("p" "q"))
                (conclusion "p^q")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Adición")
                (listaPremisas '("p"))
                (conclusion "pvq")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Separación")
                (listaPremisas '("p" "p->q"))
                (conclusion "q")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Modus Tollendo Tollens")
                (listaPremisas '("p->q" "~q"))
                (conclusion "~p")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Modus Tollendo Ponens")
                (listaPremisas '("pvq" "~p"))
                (conclusion "q")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Silogismo Hipotetico")
                (listaPremisas '("p->q" "q->r"))
                (conclusion "p->r")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Silogismo Disjuntivo")
                (listaPremisas '("pvq" "p->r" "q->s"))
                (conclusion "rvs")))
            (set! listaInferencias (cons inferencia listaInferencias))

            (set! inferencia
            (new Inferencia
                (nombre "Idempotencia")
                (listaPremisas '("pvp"))
                (conclusion "p")))
            (set! listaInferencias (cons inferencia listaInferencias))
        )

        (define/public (set-tautologias)
            (define tautologia
                (new Tautologia
                    (nombre "Doble negación")
                    (axioma_1 "~~p")
                    (axioma_2 "p"))
            )
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Definicion de equivalencia")
                (axioma_1 "p<->q")
                (axioma_2 "(p->q)^(q->p)")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Contrapositiva")
                (axioma_1 "~p->~q")
                (axioma_2 "q->p")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Leyes de De Morgan")
                (axioma_1 "~(p^q)")
                (axioma_2 "(~pv~q)")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Leyes de De Morgan")
                (axioma_1 "~(pvq)")
                (axioma_2 "(~p^~q)")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Negacion de Implicacion")
                (axioma_1 "~(p->q)")
                (axioma_2 "(p^~q)")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Implicacion Material")
                (axioma_1 "p->q")
                (axioma_2 "~pvq")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Leyes Distributivas")
                (axioma_1 "p^(qvr)")
                (axioma_2 "(p^q)v(p^r)")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Leyes Distributivas")
                (axioma_1 "pv(q^r)")
                (axioma_2 "(pvq)^(pvr)")))
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Exportacion")
                (axioma_1 "p->(q->r)")
                (axioma_2 "(p^q)->r")))
            (set! listaTautologias (cons tautologia listaTautologias))
        )

        (super-new)
    )
)


(define p1 (new Probador))
(send p1 acepte-deduccion "p->q,~r => ~q->~p")
(send p1 prueba-deduccion)


(define (is-variable-a-letter? x)
  (let ((var (string->list (symbol->string x))))
    (and (= (length var) 1)
         (char-alphabetic? (car var)))))