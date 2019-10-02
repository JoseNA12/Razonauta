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
            (evaluarExpresionRegular expresion)

        )

        (define/public (evaluarExpresionRegular expresion)
            ; Sacar el patron que sigue la expresion y apartir de eso 
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


(define pb (new Probador))
(send pb acepte-deduccion "p->q,~r => ~q->~p")