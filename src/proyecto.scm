#lang scheme

(require (lib "class.ss"))

(define Probador
    (class object%
        (field 
            ;(deduccion null)
            (estructuraPremisas null)
            (estructuraConclusiones null)
            (raiz null)
        )

        (define/public (acepte-deduccion pExpresion)
            (define p1 (new Parser))
            (define tupla (send p1 parsearExpresion pExpresion)) ; tupla con los bloques de la expresion

            (cond
                ((not (equal? tupla #f)) ; no existen errores en la deducción ingresada, continúe
                    (define lstBloquesPremisas (car tupla))
                    (define lstBloquesConclusiones (car (cdr tupla)))
                    
                    (set! estructuraPremisas (send motor crearEstructura lstBloquesPremisas "premisa" '()))
                    (set! estructuraConclusiones (send motor crearEstructura lstBloquesConclusiones "conclusion" '()))
                    ;(display "estructuraPremisas: ") (display estructuraPremisas) (newline)
                    ;(display "estructuraConclusiones: ") (display estructuraConclusiones) (newline)
                    
                )
                (else #f) ; no haga nada
            )
        )

        (define/public (pruebe-deduccion)
            (define e1 (new Estratega))
            (send e1 inicializarArbol estructuraPremisas estructuraConclusiones)
            (set! raiz (send e1 getRaiz))
        )

        (define/public (arbol)
            (cond 
                ((equal? raiz null)
                    (display "Error. Pruebe la deduccion primero.") (newline)
                )
                (else 
                    (display "Arbol de solucion") (newline)
                    (pre-orden raiz)
                )
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
        (define/public (crearEstructura pPremisas pZona pTupla)      

            (cond
                ((pair? pPremisas)
                    ;(display "- Envio: ") (display (car pPremisas)) (newline)
                    (define arbolPremisa (construirEstructura (car pPremisas) pZona))

                    (cond
                        ((list? arbolPremisa)
                            (set! pTupla (append pTupla arbolPremisa))
                        )
                        (else ; premisa individual
                            (set! pTupla (append pTupla (list arbolPremisa)))
                        )
                    )
                    ;(display "- Cola: ") (display (cdr pPremisas)) (newline)
                    (crearEstructura (cdr pPremisas) pZona pTupla) ; ir iterando entre los bloques de premisas de la lista
                ) ; sin elementos, ya se recorrió todo
                (else pTupla)
            )

            ;pTupla
        )

        ; Construye un "árbol" con objetos 'Operación' de forma recursiva
        (define/public (construirEstructura expresion pZona) 
            ; p -> (p v (r ^ q))
            ; Contenido de 'lst' tiene la forma: '("expresion" "permisa" 'noImporta' "premisa")
            (cond
                ; (.) noImporta (.)
                ((regexp-match? (send BC get-regx_1) expresion)
                    (define lst (regexp-match (send BC get-regx_1) expresion))
                    ; ("~(p)->(pv(r^q))" "~" "(p)" "->" #f "(pv(r^q))")

                    (define esNegativo1_? (car (cdr lst)))
                    (define esNegativo2_? (car (cddddr lst)))

                    (define op1 (car (cddr lst)))
                    (define oper (car (cdddr lst)))
                    (define op2 (car (cdr (cddddr lst))))

                    (cond ((equal? esNegativo1_? "~")
                            (set! op1 (string-append "~" op1))
                        )
                    )

                    (cond ((equal? esNegativo2_? "~")
                            (set! op2 (string-append "~" op2))
                        )
                    )

                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura op1 pZona) (construirEstructura op2 pZona)) oper pZona)
                    )

                    ;(display "(.) noImporta (.)") (newline)
                    ;(display lst)
                    obj
                )

                ; (.) noImporta .
                ((regexp-match? (send BC get-regx_2) expresion)
                    (define lst (regexp-match (send BC get-regx_2) expresion))

                    (define esNegativo1_? (car (cdr lst)))
                    (define esNegativo2_? (car (cddddr lst)))

                    (define op1 (car (cddr lst)))
                    (define oper (car (cdddr lst)))
                    (define op2 (car (cdr (cddddr lst))))

                    (cond ((equal? esNegativo1_? "~")
                            (set! op1 (string-append "~" op1))
                        )
                    )

                    (cond ((equal? esNegativo2_? "~")
                            (set! op2 (string-append "~" op2))
                        )
                    )

                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura op1 pZona) (construirEstructura op2 pZona)) oper pZona)
                    )

                    ;(display "(.) noImporta .") (newline)
                    ;(display lst)
                    obj
                )

                ; . noImporta (.)
                ((regexp-match? (send BC get-regx_3) expresion)
                    (define lst (regexp-match (send BC get-regx_3) expresion))

                    (define esNegativo1_? (car (cdr lst)))
                    (define esNegativo2_? (car (cddddr lst)))

                    (define op1 (car (cddr lst)))
                    (define oper (car (cdddr lst)))
                    (define op2 (car (cdr (cddddr lst))))

                    (cond ((equal? esNegativo1_? "~")
                            (set! op1 (string-append "~" op1))
                        )
                    )

                    (cond ((equal? esNegativo2_? "~")
                            (set! op2 (string-append "~" op2))
                        )
                    )
                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura op1 pZona) (construirEstructura op2 pZona)) oper pZona)
                    )

                    ;(display ". noImporta (.)") (newline)
                    ;(display lst)
                    obj
                )

                ; . noImporta .
                ((regexp-match? (send BC get-regx_4) expresion)
                    (define lst (regexp-match (send BC get-regx_4) expresion))

                    (define esNegativo1_? (car (cdr lst)))
                    (define esNegativo2_? (car (cddddr lst)))

                    (define op1 (car (cddr lst)))
                    (define oper (car (cdddr lst)))
                    (define op2 (car (cdr (cddddr lst))))

                    (cond ((equal? esNegativo1_? "~")
                            (set! op1 (string-append "~" op1))
                        )
                    )

                    (cond ((equal? esNegativo2_? "~")
                            (set! op2 (string-append "~" op2))
                        )
                    )

                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura op1 pZona) (construirEstructura op2 pZona)) oper pZona)
                    )

                    ;(display ". noImporta .") (newline)
                    ;(display lst)
                    obj
                )

                ; ~(.)
                ((regexp-match? (send BC get-regx_5) expresion)
                    (define lst (regexp-match (send BC get-regx_5) expresion))

                    (define op1 (car (cdr lst)))

                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura (substring op1 1) pZona)) "~" pZona)
                    )
                    ;(construirEstructura (eliminarCaracteres (car (cddr lst))))
                    ;(display "~(.)") (newline)
                    ;(display lst)
                    obj
                )

                ; (.)
                ((regexp-match? (send BC get-regx_6) expresion)
                    (define lst (regexp-match (send BC get-regx_6) expresion))

                    (construirEstructura (eliminarCaracteres (car lst)) pZona)
                    ;(display "(.)") (newline)
                    ;(display lst)
                )

                ; ~.
                ((regexp-match? (send BC get-regx_7) expresion)
                    (define lst (regexp-match (send BC get-regx_7) expresion))
                    
                    (define op1 (car (cdr lst)))

                    (define obj (crearObjectoOperacion 
                        (list (construirEstructura (substring op1 1) pZona)) "~" pZona)
                    )

                    ;(display ".") (newline)
                    ;(display lst)
                    obj
                )

                ; .
                ((regexp-match? (send BC get-regx_8) expresion)
                    (define lst (regexp-match (send BC get-regx_8) expresion))
                    
                    ;(display ".") (newline)
                    ;(display lst)
                    (car lst)
                )

            )
        )
        ; Remueve los caracteres laterales de una expresión. Ej: "(pv(r^q))" -> "pv(r^q)" 
        (define (eliminarCaracteres pExpresion)
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

(define Estratega
    (class object%
        (field 
            (raiz null)
        )

        (define/public (getRaiz)
            raiz
        )

        ; Init
        (define/public (inicializarArbol listaPremisas listaConclusion)
            (display "estructuraPremisas: ") (display listaPremisas) (newline)
            (display "estructuraConclusiones: ") (display listaConclusion) (newline)

            (set! raiz(new raiz% (premisa listaPremisas) (conclusion listaConclusion)))
            (procesarNodo raiz)
        )
        
        ; Procesa la premisa hasta que no pueda realizar ninguna operacion, luego sigue con la conclusion.
        (define/public (procesarNodo nodo)
            (define premisas (send nodo get-premisa))
            (define conclusion (send nodo get-conclusion))
            (cond 
                ((equal? (procesarArgumento nodo premisas "premisa" conclusion) null)
                    (procesarArgumento nodo conclusion "conclusion" premisas)
                )
            )
        )

        ; Recorre cada expresion del argumento en busca de alguno que pueda ser modificado.
        (define/public (procesarArgumento nodo argumento zonaArgumento contraArgumento)
            (define result (procesarExpresionesArgumento argumento  (list (list ))) )
            (cond 
                ((equal? result null)
                    null
                )
                (else 
                    (define rama (car result))
                    (define regla (cadr result))
                    (define formula (caddr result))
                    (define nuevaZona (cadddr result))
                    (cond 
                        ((equal? (length rama) 2)
                            (generarHijos 1 nodo (car rama) contraArgumento formula regla zonaArgumento)
                            (generarHijos 2 nodo (cadr rama) contraArgumento formula regla zonaArgumento)
                        )
                        (else 
                            (cond
                                ((equal? nuevaZona zonaArgumento)
                                    (generarHijos 1 nodo (car rama) contraArgumento formula regla zonaArgumento)
                                )
                                (else 
                                    (generarHijos 1 nodo (car (car rama)) (append contraArgumento (cadr (car rama)))  formula regla zonaArgumento)
                                )
                            )
                        )
                    )
                )
            )
        )

        ; Procesa cada expresion contenida en el argumento con el fin de realizar el cambio. Ramas representa la nueva rama que saldra del arbol
        (define/public (procesarExpresionesArgumento argumento ramas)
            (cond 
                ((equal? argumento '())
                    null
                )
                (else 
                    (define operacion (car argumento))
                    (cond
                        ((string? operacion)
                            (set! ramas (list (append (car ramas) (list operacion))))
                            (procesarExpresionesArgumento (cdr argumento) ramas)
                        )
                        (else 
                            (generarNuevoArgumento ramas operacion argumento)
                        )
                    )
                )
            )
        )

        ; Crea nuevos hijos para el nodo padre y continua con la recursion de procesamiento para estos nodos hijos. 
        (define/public (generarHijos ladoRama nodoPadre argumentoCambiado contraArgumento formula regla zonaArgumento)
            (cond
                ((equal? zonaArgumento "premisa")
                    (procesarNodo (expandirRama ladoRama nodoPadre argumentoCambiado contraArgumento formula regla))
                )
                (else
                    (procesarNodo (expandirRama ladoRama nodoPadre contraArgumento argumentoCambiado formula regla))
                )
            )
        )

        ; Asigna el nodo hijo a su padre en el lado indicado
        (define/public (expandirRama ladoRama nodoPadre premisa conclusion formula regla)
            (define nodoHijo (crearNodo premisa conclusion formula regla))
            (cond 
                ((equal? ladoRama 1)
                    (send nodoPadre insert-izq nodoHijo)
                )
                (else 
                    (send nodoPadre insert-der nodoHijo)
                )
            )
            nodoHijo
        )

        (define/public (crearNodo premisa conclusion regla formula)
            (define nodo (new nodo% (premisa premisa) (conclusion conclusion)))
            (send nodo set-formula formula)
            (send nodo set-regla regla)
            nodo
        )

        ; Toma la operacion y realiza la transformacion correspondiente. Retorna el argumento en su totalidad junto con el cambio de operacion realizado.
        (define/public (generarNuevoArgumento ramas operacion argumento)
            (define zona (send operacion get-zona))
            (define result (modificarOperacion zona operacion))
            (define regla (car result))
            (define formula operacion)
            (define nuevaZona (cadr result))
            (define nuevaOperacion (cdr (cdr result))) 
            (cond 
                ((or (equal? regla "and-der") (equal? regla "or-izq"))
                    (define ramaHija1 (list (append (append (car ramas) (list (car nuevaOperacion))) (cdr argumento))))
                    (define ramaHija2 (list (append (append (car ramas) (list (cadr nuevaOperacion))) (cdr argumento))))
                    (set! ramas (list (car ramaHija1) (car ramaHija2)))
                )
                (else  
                    (cond 
                        ((equal? nuevaZona zona)
                            (set! ramas (list (append (append (car ramas) nuevaOperacion) (cdr argumento))))
                        )
                        (else 
                            (define ramaZona1 (list (append (car ramas) (cdr argumento))))
                            (define ramaZona2 (list nuevaOperacion))
                            (set! ramas (list (list (car ramaZona1) (car ramaZona2))))
                        )
                    )
                )
            )
            (list ramas regla formula nuevaZona)
        )

        ; Se realizan las modificaciones correspondientes a la operacion y su lado. 
        (define/public (modificarOperacion zona operacion)
            (define result null)
            (cond
                ((equal? zona "premisa")
                    (set! result (getCambioOperacionIzquierda operacion))
                )
                (else
                    (set! result (getCambioOperacionDerecha operacion))  
                )
            )
            result
        )

        (define/public (getCambioOperacionIzquierda operacion)
            (define operador (send operacion get-operador))
            (define operandos (send operacion get-operandos))
            (cond 
                ((equal? operador "^")
                    (dobleOperando operandos "and-izq" "premisa")
                )
                ((equal? operador "v")
                    (dobleOperando operandos "or-izq" "premisa")
                ) 
                ((equal? operador "->")
                    (implica operandos "implies-izq" "premisa")
                )
                ((equal? operador "~")
                    (negativo (car operandos) "neg-izq" "premisa" "conclusion")
                )
            )
        )

        (define/public (getCambioOperacionDerecha operacion)
            (define operador (send operacion get-operador))
            (define operandos (send operacion get-operandos))
            (cond 
                ((equal? operador "^")
                    (dobleOperando operandos "and-der" "conclusion")
                )
                ((equal? operador "v")
                    (dobleOperando operandos "or-der" "conclusion")
                ) 
                ((equal? operador "->")
                    (implica operandos "implies-der" "conclusion")
                )
                ((equal? operador "~")
                    (negativo (car operandos) "neg-der" "conclusion" "premisa")
                )
            )
        )

        ; Transforma las operaciones AND y OR en doble operacion
        (define/public (dobleOperando operandos regla zona)
            (define operando_1 (car operandos))
            (define operando_2 (cadr operandos))
            (list regla zona operando_1 operando_2) 
        )

        ; Transforma las operaciones implica
        (define/public (implica operandos regla zona)
            (define operando_1 (car operandos))
            (define nuevoOperando_1 (crearOperacion (list operando_1) "~" zona))
            (define operando_2 (cadr operandos))
            (define nuevaOperacion (crearOperacion (list nuevoOperando_1 operando_2) "v" zona))
            (list regla zona nuevaOperacion)
        )

        ; Transforma las operaciones negativas
        (define/public (negativo operando regla zonaActual zonaContraria)
            (define zonaNueva zonaContraria)
            (cond 
                ((not (string? operando)) ; ~ (.)
                    (define operadorInterno (send operando get-operador))
                    (cond
                        ((equal? operadorInterno "~")  ; ~~(.) => (.)
                            (set! operando (car (send operando get-operandos)))
                            (set! zonaNueva zonaActual)
                        )
                    )
                )
            )
            (list regla zonaNueva operando)
        )

        (define/public (crearOperacion pOperandos pOperador pZona)
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
            (regx_1 "(~)?(\\([^.]+\\))(->|<->|v|\\^)(~)?(\\([^.]+\\))$")
            ; (.) noImporta .
            (regx_2 "(~)?(\\([^.]+\\))(->|<->|v|\\^)(~)?([a-uw-z]+)$")
            ; . noImporta (.)
            (regx_3 "(~)?([a-uw-z]+)(->|<->|v|\\^)(~)?(\\([^.]+\\))$")
            ; . noImporta .
            (regx_4 "(~)?([a-uw-z]+)(->|<->|v|\\^)(~)?([a-uw-z]+)$")
            ; ~(.)
            (regx_5 "(^~\\([^.]+\\))$")
            ; (.)
            (regx_6 "(^\\([^.]+\\))$")
            ; ~.
            (regx_7 "(^~[a-uw-z]+)$")
            ; .
            (regx_8 "(^[a-uw-z]+)$")
        )

        (define/public (get-regx_1) regx_1)
        (define/public (get-regx_2) regx_2)
        (define/public (get-regx_3) regx_3)
        (define/public (get-regx_4) regx_4)
        (define/public (get-regx_5) regx_5)
        (define/public (get-regx_6) regx_6)
        (define/public (get-regx_7) regx_7)
        (define/public (get-regx_8) regx_8)

        (super-new)
    )
)

;============================================================
;============================================================

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

;============================================================
;============================================================
;Raiz del ábol
(define raiz%
    (class object%
        (init-field premisa conclusion)
        
        (field (izq null) (der null))
        
        (define/public (insert-izq node)
            (set! izq node)
        )

        (define/public (get-node-izq) izq)  

        (define/public (insert-der node)
            (set! der node)
        )

        (define/public (get-node-der) der)

        (define/public (get-premisa) premisa)
        
        (define/public (get-conclusion) conclusion)
        
        (super-new)
    )
)

;Nodos del árbol (herencia)
(define nodo%
    (class raiz%
        (inherit-field premisa conclusion)
        
        (field (formula null) (regla null))

        (define/public (set-formula nueva-formula)
            (set! formula nueva-formula))

        (define/public (set-regla nueva-regla)
            (set! regla nueva-regla))

        (define/public (to-string)
            (if (and (not (null? formula))(not (null? regla)))
                (string-append "Nodo : " premisa conclusion "   |-" formula "   |-"regla)
                (string-append "Nodo : " premisa conclusion)))
        
        (define/public (print-nodo)
            (define nodo-completo (send this to-string))
            (printf nodo-completo))
            
        (super-new)
    )
)

(define (pre-orden arbol)
    (cond ((not (null? arbol))
            (display (send arbol get-premisa)) (newline)
            (pre-orden (send arbol get-node-izq))
            (pre-orden (send arbol get-node-der))   
        )
    )
)

; ================================================================ ;

(define BC (new BaseConocimiento))
(define motor (new Motor))

(define pb (new Probador))
(send pb acepte-deduccion "~p->~q,~r => ~q->~p")

;estructuraPremisas: (#(struct:object:Operacion% ...) #(struct:object:Operacion% ...) #(struct:object:Operacion% ...))
;estructuraConclusiones: (#(struct:object:Operacion% ...) #(struct:object:Operacion% ...) #(struct:object:Operacion% ...))

;(send pb acepte-deduccion "~q,r^p,p->(pv(r^q)) => ~w->~b,y^p,~x")
(send pb pruebe-deduccion)
(send pb arbol)

; ================================================================ ;