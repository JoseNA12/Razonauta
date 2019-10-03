#lang scheme

(require (lib "class.ss"))

(define Probador
    (class object%
        (field 
            (deduccion null)
        )

        (define/public (acepte-deduccion pExpresion)
            (define p1 (new Parser))
            (send p1 parseExpression pExpresion)
        )

        (super-new)
    )
)


(define Parser
    (class object%
      
        (define/public (parseExpression pExpresion)
             (define expresionSeparada (separarExpresion pExpresion))
             (parsearOperaciones expresionSeparada)
        )

        (define/public (separarExpresion pExpresion)
            (define listaExp (string-split pExpresion))
            (define tempDeduc '())

            (cond 
                ((> (length listaExp) 0)
                    (set! tempDeduc (append tempDeduc (car listaExp)))
                    (cond 
                        ((>= (length listaExp) 3)
                            (set! tempDeduc (cons tempDeduc (car (cdr (cdr listaExp)))))
                        )
                        (else 
                            ("Expresión incompleta")
                        )
                    ) 

                ) 
                (else 
                    (display "No se ingresó una expresión")
                )
            )

            tempDeduc
        )

        (define/public (parsearOperaciones expresion)
            (define premisa (car expresion))
            (define conclucion (cdr expresion))
            (display premisa)
            (newline)
          
            (generarOperaciones premisa)
            (display conclucion)
            (newline)
            (generarOperaciones conclucion)
        )

        (define/public (generarOperaciones expresion)
            ;(set! expresion (string->list expresion))
            (set! expresion (str-split expresion #\,))
            (display expresion)
            (newline)
            (display (car expresion))
            (newline)
            (define operacion (evaluarExpresionRegular (car expresion)))
            (newline)
            (display "operandos ")
            (display (send operacion get-operandos))
            (newline)
            (display "negativo ")
            (define operandos (send operacion get-operandos))
            (display (send (car operandos) get-operandos))
            (newline)
          ;  (evaluarExpresionRegular (car(cdr expresion)))

        )

        (define/public (evaluarExpresionRegular expresion)
            ; Sacar el patron que sigue la expresion y apartir de eso
            (define listaExp (string->list expresion))
            (cond
                ((regexp-match? ".->(." expresion)  
                    (display " Implica, parentesis segundo ") 
                    (newline)
                    (display expresion) 
                    (define operador1 (getElementoIndice 0 listaExp))
                    (define parentesis (substring expresion 4)) ; caracter despues del parentesis 
                    (define objParentesis (evaluarExpresionRegular parentesis))
                    (define implica (crearObjectoOperacion (list objParentesis operador1) "->" "zona"))
                    implica
                    ; p -> (p v (r ^ q)) p-> Obj  => (p v Obj => r ^ q)
                    ; p v (r ^ q))  p v Obj
                    ; r ^ q)) Obj
                )
                ((regexp-match? "~.->~." expresion)
                    (display " Implica, doble negativo ") 
                    (newline)
                    (display expresion) 
                    (define negativo1 (crearObjectoOperacion  (list (getElementoIndice 1 listaExp)) "~" "zona"))
                    (define negativo2 (crearObjectoOperacion  (list (getElementoIndice 5 listaExp)) "~" "zona"))
                    (define implica (crearObjectoOperacion (list negativo1 negativo2) "->" "zona"))
                    implica
                )
                ((regexp-match? "~." expresion)
                    (display expresion) (display " solo un negativo ") (newline)
                )
                ((regexp-match? ".->." expresion)
                    (display expresion) (display " puede aplicar: ") (newline)
                )
            )
            
        )
        (define (getTextoParentesis expresion )

        )

        (define (getElementoIndice n l)
            (if (or (> n (length l)) (< n 0))
                (error "Index out of bounds.")
                (if (eq? n 0)
                (car l)
                (getElementoIndice (- n 1) (cdr l)))))
                
        (define/public (crearObjectoOperacion pOperandos pOperador pZona)
            (define operacion
                (new Operacion% (operandos pOperandos)
                    (operador pOperador)
                    (zona pZona)
                )
            )
            operacion
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

        (define/public (es-caracter-alfabetico? x)
            (and (char? x) (char-alphabetic? x)) 
        )

        (super-new)
    )
)


(define Operacion%
   (class object%
      (init-field operandos operador zona)
    
      (define/public (get-operador)
         operador) 

     (define/public (get-operandos)
         operandos)
     
     (define/public (get-zona)
         zona)
     
      (super-new)
   )
)

(define pb (new Probador))
(send pb acepte-deduccion "~p->~q,~r => ~q->~p")

