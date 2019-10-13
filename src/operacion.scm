#lang scheme

(define raiz null)

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
 
(define (inicializarArbol listaPremisas listaConclusion)
  (display "estructuraPremisas: ") (display listaPremisas) (newline)
  (display "estructuraConclusiones: ") (display listaConclusion) (newline)

  (set! raiz(new raiz% (premisa listaPremisas) (conclusion listaConclusion)))
  (procesarNodo raiz)
)

(define (procesarNodo nodo)
  (define premisas (send nodo get-premisa))
  (define conclusion (send nodo get-conclusion))
  (display "Premisas ") (display premisas) (newline)
  (display "Conclusiones ") (display conclusion) (newline)

  (cond 
    ((equal? (procesarArgumento nodo premisas "premisa" conclusion) null)
      (display "Analizar conclusion") (newline)
      (procesarArgumento nodo conclusion "conclusion" premisas)
    )
  )
  (display "Fin")
)

(define (procesarArgumento nodo argumento zonaArgumento contraArgumento)
  (define result (procesarRamas argumento  (list (list ))) )
  ;(set! ramas (list (list (car ramaZona1) (car ramaZona2))))
  ;(list ramas regla formula nuevaZona)
  (cond 
    ((equal? result null)
      (display "No se puede procesar este argumento") (newline)
      null
    )
    (else ; ramas: ((#(struct:object:Operacion% ...) (r)))
      (define rama (car result))
      (define regla (cadr result))
      (define formula (caddr result))
      (define nuevaZona (cadddr result))
      (cond 
        ((equal? (length rama) 2)
          (display "size 2") (newline)
          (modificarArgumento 1 nodo (car rama) contraArgumento formula regla zonaArgumento)
          (modificarArgumento 2 nodo (cadr rama) contraArgumento formula regla zonaArgumento)
        )
        (else 
          (cond
            ((equal? nuevaZona zonaArgumento)
              (modificarArgumento 1 nodo (car rama) contraArgumento formula regla zonaArgumento)
            )
            (else 
              (display "Negativo, pasando de lado")
              (modificarArgumento 1 nodo (car (car rama)) (append contraArgumento (cadr (car rama)))  formula regla zonaArgumento)
            )
          )
        )
      )
    )
  )
)

(define (procesarRamas argumento ramas)
  (cond 
    ((equal? argumento '())
      null
    )
    (else 
      (define operacion (car argumento))
      (cond
        ((string? operacion)
          (set! ramas (list (append (car ramas) (list operacion))))
          (procesarRamas (cdr argumento) ramas)
        )
        (else 
          (generarRamas ramas operacion argumento)
            ;(set! ramas (list (list (car ramaZona1) (car ramaZona2))))
            ;(list ramas regla formula nuevaZona)
        )
      )
    )
  )
)

(define (generarRamas ramas operacion argumento)
  (define zona (send operacion get-zona))
  (define result (modificarOperacion zona operacion))
  ; (list regla zonaNueva operando)
  
  (define regla (car result))
  (define formula operacion)
  (define nuevaZona (cadr result))
  (define nuevaOperacion (cdr (cdr result))) 
  (cond 
    ((or (equal? regla "and-der") (equal? regla "or-izq"))
      (define ramaHija1 (list (append (append (car ramas) (list (car nuevaOperacion))) (cdr argumento))))
      (define ramaHija2 (list (append (append (car ramas) (list (cadr nuevaOperacion))) (cdr argumento))))
      (set! ramas (list (car ramaHija1) (car ramaHija2)))
      (display ramas) (newline)
    )
    (else  ; (append y x)
      (cond 
        ((equal? nuevaZona zona)
          (set! ramas (list (append (append (car ramas) nuevaOperacion) (cdr argumento))))
          (display ramas) (newline)
        )
        (else 
          (display "negativo carepicha")
          (define ramaZona1 (list (append (car ramas) (cdr argumento))))
          (define ramaZona2 (list nuevaOperacion))
          (newline)
          (display "ramaZona1")
          (display ramaZona1)
          (newline)
          (set! ramas (list (list (car ramaZona1) (car ramaZona2))))
          (display "ramas: ")
          (display ramas)
          (newline)
        )
      )
    )
  )
  (list ramas regla formula nuevaZona)
)

(define (modificarArgumento ladoRama nodoPadre argumentoCambiado contraArgumento formula regla zonaArgumento)
  (cond
    ((equal? zonaArgumento "premisa")
      (procesarNodo (expandirRama ladoRama nodoPadre argumentoCambiado contraArgumento formula regla))
    )
    (else
      (procesarNodo (expandirRama ladoRama nodoPadre contraArgumento argumentoCambiado formula regla))
    )
  )
)

(define (expandirRama ladoRama nodoPadre premisa conclusion formula regla)
  (display "Agregando hijo a la rama ") (display ladoRama) (newline)
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

(define (crearNodo premisa conclusion regla formula)
  (define nodo (new nodo% (premisa premisa) (conclusion conclusion)))
  (send nodo set-formula formula)
  (send nodo set-regla regla)
  nodo
)

(define (modificarOperacion zona operacion)
  (define result null)
  (cond
    ((equal? zona "premisa")
      (set! result (getCambioOperacionIzquierda operacion))
    )
    (else
      (set! result (getCambioOperacionDerecha operacion))  
      ; (list regla zona (list operando_1 operando_2))
    )
  )
  result
)

(define (getCambioOperacionIzquierda operacion)
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

(define (getCambioOperacionDerecha operacion)
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
      (define x (implica operandos "implies-der" "conclusion"))
      (display x)
      x
    )
    ((equal? operador "~")
      (negativo (car operandos) "neg-der" "conclusion" "premisa")
    )
  )
)

(define (dobleOperando operandos regla zona)
  (define operando_1 (car operandos))
  (define operando_2 (cadr operandos))
  (display regla) (newline)
  (list regla zona operando_1 operando_2) 
)

(define (implica operandos regla zona)
  (define operando_1 (car operandos))
  (define nuevoOperando_1 (crearOperacion (list operando_1) "~" zona))
  (define operando_2 (cadr operandos))
  (define nuevaOperacion (crearOperacion (list nuevoOperando_1 operando_2) "v" zona))
  (display regla) (newline)
  (list regla zona nuevaOperacion)
)

(define (negativo operando regla zonaActual zonaContraria)
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

(define (crearOperacion pOperandos pOperador pZona)
    (define operacion
        (new Operacion% (operandos pOperandos)
            (operador pOperador)
            (zona pZona)
        )
    )
    operacion
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
            (newline)
            (display "Premisa: ") (display (send arbol get-premisa)) (display " Conclusion: ") (display (send arbol get-conclusion)) 
            (pre-orden (send arbol get-node-izq))
            (pre-orden (send arbol get-node-der))   
        )
    )
)



;============================================================
;============================================================

(define premisa '())
(define conclusion '())

;============================================================
; (p -> q) => ((~ q) -> (~ p))
;============================================================

(define operacion
  (new Operacion% (operandos '("p" "q"))
       (operador "->")
       (zona "premisa")
  )
)



;============================================================
;============================================================
(define operacion4
  (new Operacion% (operandos '("p" "q"))
       (operador "v")
       (zona "conclusion")
  )
)

(define operacion6
  (new Operacion% (operandos '("p" "r"))
       (operador "^")
       (zona "premisa")
  )
)

(set! operacion
  (new Operacion% (operandos '("q"))
       (operador "~")
       (zona "premisa")
  )
)

(define operacion10
  (new Operacion% (operandos '("q"))
       (operador "~")
       (zona "premisa")
  )
)

(define qNegConclu
  (new Operacion% (operandos '("q"))
       (operador "~")
       (zona "conclusion")
  )
)

(define pNegConclu
  (new Operacion% (operandos '("p"))
       (operador "~")
       (zona "conclusion")
  )
)

(define implicaDobleNegativoConclu
  (new Operacion% (operandos (list qNegConclu pNegConclu))
       (operador "->")
       (zona "conclusion")
  )
)

(define operacion2
  (new Operacion% (operandos '("p"))
       (operador "~")
       (zona "premisa")
  )
)

(define rNegPremisa
  (new Operacion% (operandos '("r"))
       (operador "~")
       (zona "premisa")
  )
)

(define implicaDobleNegativo
  (new Operacion% (operandos (list operacion2 operacion10))
       (operador "->")
       (zona "premisa")
  )
)

(define operacion7
  (new Operacion% (operandos (list "r" operacion10))
       (operador "->")
       (zona "conclusion")
  )
)

(set! premisa (append premisa implicaDobleNegativo))
(set! premisa (cons rNegPremisa premisa ))

(display premisa) 
(set! conclusion (cons implicaDobleNegativoConclu conclusion ))
;============================================================
; ((~q -> ~p), (p ^ r) -> (p v q), (r -> ~q))
;============================================================

;"~p->~q,~r => ~q->~p
; ""
(inicializarArbol premisa conclusion)
(pre-orden raiz)