#lang scheme

(require (lib "class.ss"))


(define Probador
    (class object%
        (field 
            (deduccion null)
        )

        (define/public (acepte-deduccion pExpresion)
            (define p1 (new Parser))
            (define tupla (send p1 parsearExpresion pExpresion)) ; tupla con los bloques de la expresion

            (cond
                ((not (equal? tupla #f)) ; no existen errores en la deducción ingresada, continúe
                    (define lstBloquesPremisas (car tupla))
                    (define lstBloquesConclusiones (car (cdr tupla)))
                    
                    (define estructuraPremisas (send motor crearEstructura lstBloquesPremisas '()))
                    (define estructuraConclusiones (send motor crearEstructura lstBloquesConclusiones '()))
                    (display "estructuraPremisas: ") (display estructuraPremisas) (newline)
                    (display "estructuraConclusiones: ") (display estructuraConclusiones) (newline)
                )
                (else #f) ; no haga nada
            )
        )

        (super-new)
    )
)


(define Parser
    (class object%
      
        ; Retorna una tupla con los bloques de la expresion (1- premisas y 2- conclusiones)
        (define/public (parsearExpresion pExpresion)
            (define expresionSeparada (separarExpresion pExpresion))

            (cond
                ((not (equal? expresionSeparada #f))
                    (define lstPremisas (obtenerBloquesExpresion (car expresionSeparada)))
                    (define lstConclusiones (obtenerBloquesExpresion (cdr expresionSeparada)))
                    (list lstPremisas lstConclusiones) ; return
                )
                (else
                    (display "La expresión ingresada presenta errores")
                    #f
                )
            )
        )

        ; Retorna una lista con la expresion separa en premisa y conclusion
        (define/public (separarExpresion pExpresion)
            (define listaExp (string-split pExpresion))
            (define tempDeduc '())

            (cond 
                ((> (length listaExp) 0)
                    (set! tempDeduc (append tempDeduc (car listaExp)))
                    (cond 
                        ((>= (length listaExp) 3)
                            (set! tempDeduc (cons tempDeduc (car (cdr (cdr listaExp)))))
                            tempDeduc
                        )
                        (else #f)
                    )
                )
                (else #f)
            )
        )

        ; Retorna una lista con la expresion dada separada segun las comas
        (define/public (obtenerBloquesExpresion expresion)
            (str-split expresion #\,)
        )

        ; Separa un string segun el caracter indicado
        ; FUENTE: https://gist.github.com/matthewp/2324447
        (define/public (str-split str ch) 
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
    )
)


(define Motor
    (class object%

        ; Construir una estructura lista que contiene objetos 'Operacion' en forma de "árbol"
        (define/public (crearEstructura pPremisas pTupla)      

            (cond
                ((pair? pPremisas)
                    ;(display "- Envio: ") (display (car pPremisas)) (newline)
                    (define arbolPremisa (construirEstructura (car pPremisas)))

                    (cond
                        ((list? arbolPremisa)
                            (set! pTupla (append pTupla arbolPremisa))
                        )
                        (else ; premisa individual
                            (set! pTupla (append pTupla (list arbolPremisa)))
                        )
                    )
                    ;(display "- Cola: ") (display (cdr pPremisas)) (newline)
                    (crearEstructura (cdr pPremisas) pTupla) ; ir iterando entre los bloques de premisas de la lista
                ) ; sin elementos, ya se recorrió todo
                (else pTupla)
            )

            ;pTupla

            ; (define arbolOperacion (construirEstructura (car pPremisas)))
            ; (display arbolOperacion)
            ; (newline)
        )

        ; Construye un "árbol" con objetos 'Operación' de forma recursiva
        (define/public (construirEstructura expresion) 
            ; p -> (p v (r ^ q))
            ; Contenido de 'lst' tiene la forma: '("expresion" "permisa" 'noImporta' "premisa")
            (cond
                ; (.) noImporta (.)
                ((regexp-match? (send BC get-regx_1) expresion)
                    (define lst (regexp-match (send BC get-regx_1) expresion))

                    (define op1 (car (cdr lst)))
                    (define oper (car (cddr lst)))
                    (define op2 (car (cdddr lst)))
                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura op1) (construirEstructura op2)) oper "zona")
                    )
                    obj
                    ; (display "(.) noImporta (.)") (newline)
                    ; (display lst)   
                )

                ; (.) noImporta .
                ((regexp-match? (send BC get-regx_2) expresion)
                    (define lst (regexp-match (send BC get-regx_2) expresion))

                    (define op1 (car (cdr lst)))
                    (define oper (car (cddr lst)))
                    (define op2 (car (cdddr lst)))
                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura op1) op2) oper "zona")
                    )
                    obj
                    ; (display "(.) noImporta .") (newline)
                    ; (display lst)
                )

                ; . noImporta (.)
                ((regexp-match? (send BC get-regx_3) expresion)
                    (define lst (regexp-match (send BC get-regx_3) expresion))

                    (define op1 (car (cdr lst)))
                    (define oper (car (cddr lst)))
                    (define op2 (car (cdddr lst)))
                    (define obj (crearObjectoOperacion 
                        (list op1 (construirEstructura op2)) oper "zona")
                    )
                    obj
                    ; (display ". noImporta (.)") (newline)
                    ; (display lst)
                )

                ; . noImporta .
                ((regexp-match? (send BC get-regx_4) expresion)
                    (define lst (regexp-match (send BC get-regx_4) expresion))

                    (define op1 (car (cdr lst)))
                    (define oper (car (cddr lst)))
                    (define op2 (car (cdddr lst)))
                    (define obj (crearObjectoOperacion (list op1 op2) oper "zona"))
                    obj
                    ; (display ". noImporta .") (newline)
                    ; ( display lst)
                )

                ; (.)
                ((regexp-match? (send BC get-regx_5) expresion)
                    (define lst (regexp-match (send BC get-regx_5) expresion))

                    (construirEstructura (eliminarParentesis (car lst)))
                    ; (display "(.)") (newline)
                    ; (display lst)
                )

                ; .
                ((regexp-match? (send BC get-regx_6) expresion)
                    (define lst (regexp-match (send BC get-regx_6) expresion))
                    ; (display ".") (newline)
                    ;( display lst)

                    (car lst)
                )
            )
        )
        ; Remueve los parentesis laterales de una expresión. Ej: "(pv(r^q))" -> "pv(r^q)" 
        (define (eliminarParentesis pExpresion)
            (define x (substring pExpresion 1))
            (substring x 0 (- (string-length x) 1))
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

        (super-new)
    )
)


(define BaseConocimiento
    (class object%

        ; [a-uw-z~]: Se incluye cualquier letra del abecedario excepto 'v' e incluyendo '~'
        ; '^' al inicio indica que se debe empezar por ese caracter
        ; '$' al final indica que coincide solo al final de la cadena (hace más estricto al patrón)

        (field
            ; (.) noImporta (.)
            (regx_1 "(^\\([^.]+\\))(->|<->|v|\\^)(\\([^.]+\\))$")
            ; (.) noImporta .
            (regx_2 "(^\\([^.]+\\))(->|<->|v|\\^)([a-uw-z~]+)$")
            ; . noImporta (.)
            (regx_3 "(^[a-uw-z~]+)(->|<->|v|\\^)(\\([^.]+\\))$")
            ; . noImporta .
            (regx_4 "(^[a-uw-z~]+)(->|<->|v|\\^)([a-uw-z~]+)$")
            ; (.)
            (regx_5 "(^\\([^.]+\\))$")
            ; .
            (regx_6 "(^[a-uw-z~]+)$")
        )

        (define/public (get-regx_1) regx_1)
        (define/public (get-regx_2) regx_2)
        (define/public (get-regx_3) regx_3)
        (define/public (get-regx_4) regx_4)
        (define/public (get-regx_5) regx_5)
        (define/public (get-regx_6) regx_6)

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

; ================================================================ ;

(define BC (new BaseConocimiento))
(define motor (new Motor))

(define pb (new Probador))
;(send pb acepte-deduccion "~p->~q,~r => ~q->~p")
(send pb acepte-deduccion "~q,r^p,p->(pv(r^q)) => ~w->~b,y^p,~x")

; ================================================================ ;

; PRUEBAS

;(display (send motor construirEstructura "p->(pv(r^q))"))