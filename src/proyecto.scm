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
            ; p -> (p v (r ^ q)) 
            (cond
                ((regexp-match? "(.)" expresion)
                    (display expresion) 
                    (display " Parentesis ") 
                    (newline)
                    (define inicioString (getPosition (string->list expresion) #\( 0)))
                    (display inicioString)
                    
                    (display (substring expresion 0 inicioString))
                   ; (define op1 (evaluarExpresionRegular (substring expresion inicioString)))
                    ;op1
                )
                ((regexp-match? ".\\^." expresion)
                    (display expresion) (display " Operacion AND ") (newline)
                    (define inicioString (getPosition (string->list expresion) #\^ 0))
                    (define op1 (evaluarExpresionRegular (substring expresion 0 inicioString)))
                    (define op2 (evaluarExpresionRegular (substring expresion (+ inicioString 1))))
                    (define opAnd (crearObjectoOperacion (list op1 op2) "^" "zona"))
                )
                ((regexp-match? ".v." expresion)
                    (display expresion) (display " Operacion OR ") (newline)
                    (define inicioString (getPosition (string->list expresion) #\v 0))
                    (define op1 (evaluarExpresionRegular (substring expresion 0 inicioString)))
                    (define op2 (evaluarExpresionRegular (substring expresion (+ inicioString 1))))
                    (define opOr (crearObjectoOperacion (list op1 op2) "v" "zona"))
                )
                ((regexp-match? ".->." expresion)
                    (display expresion) (display " Operacion Implica ") (newline)
                    (define inicioString (getPosition (string->list expresion) #\- 0))
                    (define op1 (evaluarExpresionRegular (substring expresion 0 inicioString)))
                    (define op2 (evaluarExpresionRegular (substring expresion (+ inicioString 1))))
                    (define implica (crearObjectoOperacion (list op1 op2) "->" "zona"))
                )
                ((regexp-match? "~." expresion)
                    (display expresion) (display " Operacion Negativa ") (newline)
                    (define op1 (evaluarExpresionRegular (substring expresion 1)))
                    (define negativo (crearObjectoOperacion op1 "~" "zona"))
                    negativo
                )
                ((and (regexp-match? "." expresion) (equal? (string-length expresion) 1))
                    (display expresion) (display " Variable ") (newline)
                    expresion
                )
            )
            
        )

        (define (getPosition char-list char pos)
            (cond ((null? char-list) #f)              ; list was empty
                ((char=? char (car char-list)) pos) ; we found it!
                (else (getPosition (cdr char-list) char (add1 pos)))
            )
        ) 


        (define (getElementoIndice n l)
            (if (or (> n (length l)) (< n 0))
                (error "Index out of bounds.")
                (if (eq? n 0)
                (car l)
                (getElementoIndice (- n 1) (cdr l))))
        )
                
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
                            (split 0 0)))
        )

        (define/public (es-caracter-alfabetico? x)
            (and (char? x) (char-alphabetic? x)) 
        )

        (super-new)
    ))


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

