#lang scheme

; ⮕ ⟶ ⟷ 

(require (lib "class.ss"))
 

(define Probador
    (class object%
        (field 
            (deduccion null)
            (_arbol null)
        )

        ;; -------------- METODOS PRINCIPALES -------------- ;;

        (define/public (acepte-deduccion pExpresion)

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

        (define/public (pruebe-deduccion)
            
            ; llamar a MotorPrueba
            (define m1 (new MotorPrueba))
            (send m1 probar-deduccion deduccion)
        )

        (define/public (arbol)
            (display "arbol")
        )



        ;; ----------------- ;;
        (super-new)
    )
)


(define Parser
    (class object%
    
        ; Dar formato a la entrada del usuario, al momento de hacer acepte-deduccion
        (define/public (get-deduccion-root pExpresion)
             (define deduccion (separarExpresion pExpresion))
          
        )

        (define/public (separarExpresion pExpresion)
             ; (p->q) => ((~q)->(~p)) - LO PASA A LISTA - "(p->q)" "=>" "((~q)->(~p))"
            (define listaExp (string-split pExpresion))
            (define tempDeduc '())

            (cond ((> (length listaExp) 0)
                    (set! tempDeduc (cons (car listaExp) tempDeduc))
                    (cond ((>= (length listaExp) 3)
                            (set! tempDeduc (cons tempDeduc (car (cdr (cdr listaExp)))))
                        )
                    ) ;(else ("Expresión incompleta"))
                ) ;(else (display "No se ingresó una expresión"))
            
            )
            ; return
            (list tempDeduc)
        )

        (define/public (parsearOperaciones expresion)
            (define premisa (car expresion))
            (define conclucion (car (cdr listaExp))) 

            (display premisa)
            (display conclucion)


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
        (field
            (raizArbol null)
        )
        (define/public (probar-deduccion pDeduccion)
            (set! raizArbol(new raiz% (premisa (send pDeduccion get-premisa)) (conclusion (send pDeduccion get-conclusion))))
            (analizar-deduccion (send pDeduccion get-premisa) (send pDeduccion get-conclusion) raizArbol)
        )
; Adaptar info y enviarla al estratega
        (define/public (analizar-deduccion premisa conclusion nodo)
            (define lstPremisas (str-split premisa #\,))
            (define lstConclusiones (str-split conclusion #\,))
          
            (for ([premisa lstPremisas]) ; iterator binding
                (define result (send ET aplicar-tautologia premisa))
                (cond
                  (not (equal? "" result)
                        (send this construir-arbol result)))
            )
        )
; Implementar veredicto del estratega
        (define/public (construir-arbol-izquierda argumento nodo)
            (cond
              ((equal? "and-izq" (car regla))
                  (define nodoIzq (new nodo% (premisa "") (conclusion "")))
                  (send nodoIzq set-formula "")
                  (send nodoIzq set-regla "AND - Izquierda")
                  (send nodoActual insert-izq nodoIzq)
                  (send nodoActual print-nodo)
                  (analizar-deduccion (send nodoIzq get-premisa)  (send nodoIzq get-conclusion) nodoIzq))
                  
              ((equal? "or-izq" (car regla))
                  (define nodoDer (new nodo% (premisa "") (conclusion "")))
                  (send nodoDer set-formula "")
                  (send nodoDer set-regla "OR - Izquierda")
                  (send nodoActual insert-der nodoDer)

                  (analizar-deduccion (send nodoDer get-premisa)  (send nodoDer get-conclusion) nodoDer)
                  
                  (define nodoIzq (new nodo% (premisa "") (conclusion "")))
                  (send nodoIzq set-formula "")
                  (send nodoIzq set-regla "OR - Izquierda")
                  (send nodoActual insert-izq nodoIzq)
                  (send nodoActual print-nodo)

                  (analizar-deduccion (send nodoIzq get-premisa)  (send nodoIzq get-conclusion) nodoIzq))
            )
          )

       (define/public (construir-arbol-derecha regla nodo)
            (cond
              ((equal? "and-der" (car regla))
                  (define nodoDer (new nodo% (premisa "deduccion de nodo1") (conclusion "sadasd")))
                  (send nodoDer set-formula "formula nodo1")
                  (send nodoDer set-regla "regla nodo1")
                  (send nodoActual insert-der nodoDer)

                  (analizar-deduccion (send nodoDer get-premisa)  (send nodoDer get-conclusion) nodoDer)
                  
                  (define nodoIzq (new nodo% (premisa "deduccion de nodo1") (conclusion "sadasd")))
                  (send nodoIzq set-formula "formula nodo1")
                  (send nodoIzq set-regla "regla nodo1")
                  (send nodoActual insert-izq nodoIzq)
                  (send nodoActual print-nodo)

                  (analizar-deduccion (send nodoIzq get-premisa)  (send nodoIzq get-conclusion) nodoIzq))

              ((equal? "or-izq" (car regla))
                  (define nodoDer (new nodo% (premisa "deduccion de nodo1") (conclusion "sadasd")))
                  (send nodoDer set-formula "formula nodo1")
                  (send nodoDer set-regla "regla nodo1")
                  (send nodoActual insert-der nodoDer)

                  (define nodoIzq (new nodo% (premisa "deduccion de nodo1") (conclusion "sadasd")))
                  (send nodoIzq set-formula "formula nodo1")
                  (send nodoIzq set-regla "regla nodo1")
                  (send nodoActual insert-izq nodoIzq)
                  (send nodoActual print-nodo))
            )
            ; llamar estratega

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

        ; ------------ Axiomas de Equivalencia (Tautologías) ------------ ;        

        (define/public (aplicar-tautologia pExpresion)
            (for ([i (send BC get-listaTautologias)]) ; iterator binding
                (define axioma1 (send i get-axioma-1))
                (define axioma2 (send i get-axioma-2))
                
                (cond
                    ((and (regexp-match? axioma1 pExpresion) (mismo-len? axioma1 pExpresion))
                        (display pExpresion) (display " puede aplicar: ") (display axioma2) (newline)
                    )
                    ((and (regexp-match? axioma2 pExpresion) (mismo-len? axioma2 pExpresion))
                        (display pExpresion) (display " puede aplicar: ") (display axioma1) (newline)
                    )
                    ;(else (display "picha"))
                )
                
            )
        )

        (define (mismo-len? a b)
            (if (= (string-length a) (string-length b))
                #t
                #f)
        )


        (super-new)
    )
)


;                 ESTRUCTURA DE LA SOLUCIÓN ENCONTRADA                          
;Raiz del ábol
(define raiz%
   (class object%
      (init-field premisa conclusion)
     
      (field (izq '()) (der '()))
     
      (define/public (insert-izq node)
         (set! izq (cons node izq)))

      (define/public (get-node-izq)
         (car izq))

     (define/public (get-premisa)
         (premisa))
     
     (define/public (get-conclusion)
         (conclusion))
     
      (super-new)))

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
      (super-new)))

(define Justificador
    (class object%
        
        

        (super-new)
    )
)


; Fuerza Bruta
; (define Estratega
;   (class object%
;           ; ~p
;           ; p⮕q
;           ; ((pvq)⮕r)
;         (define/public (analizarArgumento argumento lado)
;             (define ladoDerecho 0)
;             (define listaArgumento (string->list argumento))
;             (define operador (cadr listaArgumento))
;             (cond
;                 ((equal? operador "^")
;                     (if (equal? lado ladoDerecho)
;                         ("and-der")
;                         ("and-izq")))
;                 ((equal? operador "⮕")
;                         ("implica"))
;                 ((equal? operador "v")
;                     (if (equal? lado ladoDerecho)
;                         ("or-der")
;                         ("or-izq")))
;                 ((equal? operador "~")
;                     (if (equal? lado ladoDerecho)
;                         ("neg-der")
;                         ("neg-izq")))
;                 (else (display "operador no encontrado"))
;             )
;         )
;         (super-new)
;     )
; )


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
      
        (define/public (get-formula) formula) 

        (define/public (set-regla pRegla)
            (set! regla pRegla)
        )

        (define/public (get-regla) regla) 

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
            (display "Base de Conocimiento creada!.") (newline)
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
                    (axioma_1 "~~.") ; "~~p"
                    (axioma_2 ".")) ; p
            )
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Definicion de equivalencia")
                (axioma_1 ".<->.") ; "p<->q"
                (axioma_2 "(.->.)^(.->.)"))) ; (p->q)^(q->p)
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Contrapositiva")
                (axioma_1 "~.->~.") ; "~p->~q"
                (axioma_2 ".->."))) ; "q->p"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Ley de De Morgan 1")
                (axioma_1 "~(.^.)") ; "~(p^q)"
                (axioma_2 "(~.v~.)"))) ; "(~pv~q)"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Ley de De Morgan 2")
                (axioma_1 "~(.v.)") ; "~(pvq)"
                (axioma_2 "(~.^~.)"))) ; "(~p^~q)"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Negacion de Implicacion")
                (axioma_1 "~(.->.)") ; "~(p->q)"
                (axioma_2 "(.^~.)"))) ; "(p^~q)"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Implicacion Material")
                (axioma_1 ".->.") ; "p->q"
                (axioma_2 "~.v."))) ; "~pvq"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Ley Distributiva 1")
                (axioma_1 ".^(.v.)") ; "p^(qvr)"
                (axioma_2 "(.^.)v(.^.)"))) ; "(p^q)v(p^r)"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Ley Distributiva 2")
                (axioma_1 ".v(.^.)") ; "pv(q^r)"
                (axioma_2 "(.v.)^(.v.)"))) ; "(pvq)^(pvr)"
            (set! listaTautologias (cons tautologia listaTautologias))

            (set! tautologia
            (new Tautologia
                (nombre "Exportacion")
                (axioma_1 ".->(.->.)") ; "p->(q->r)"
                (axioma_2 "(.^.)->."))) ; "(p^q)->r"
            (set! listaTautologias (cons tautologia listaTautologias))
        )

        (super-new)
    )
)

; ------------ VALORES DE INICIO DEL PROGRAMA ------------ ;

; crear la base de conocimiento
(define BC (new BaseConocimiento))
(send BC init)

(define ET (new Estratega))

; -------------------------------------------------------- ;

; ------------ PRUEBAS DEL PROGRAMA ------------ ;

(define p1 (new Probador))
(send p1 acepte-deduccion "p->q,~r => ~q->~p")
(send p1 pruebe-deduccion)

; -------------------------------------------------------- ;
