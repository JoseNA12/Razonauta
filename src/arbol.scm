#lang scheme

;                 ESTRUCTURA DE LA SOLUCIÓN ENCONTRADA                          
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


(define (post-orden arbol)
    (cond ((not (null? arbol))
            (post-orden (send arbol get-node-izq))
            (post-orden (send arbol get-node-der))
            (display (send arbol get-premisa)) (newline)
        )
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


(define root (new raiz% (premisa "deduccion de raiz") (conclusion "add")))

(define node1 (new nodo% (premisa "deduccion de nodo1") (conclusion "sadasd")))
(send node1 set-formula "formula nodo1")
(send node1 set-regla "regla nodo1")

(define node2 (new nodo% (premisa "deduccion de nodo2") (conclusion "sadasd")))
(send node2 set-formula "formula nodo2")
(send node2 set-regla "formula nodo2")

(send root insert-izq node1)
(send root insert-der node2)

            ;*
        ;1     ;2

(define node3 (new nodo% (premisa "deduccion de node3") (conclusion "sadasd")))
(send node3 set-formula "formula node3")
(send node3 set-regla "regla node3")

(define node4 (new nodo% (premisa "deduccion de node4") (conclusion "sadasd")))
(send node4 set-formula "formula node4")
(send node4 set-regla "formula node4")

(send node1 insert-izq node3)
(send node1 insert-der node4)

                ;*
        ;1             ;2
    ;3     ;4

(define node5 (new nodo% (premisa "deduccion de node5") (conclusion "sadasd")))
(send node5 set-formula "formula node5")
(send node5 set-regla "regla node5")

(define node6 (new nodo% (premisa "deduccion de node6") (conclusion "sadasd")))
(send node6 set-formula "formula node6")
(send node6 set-regla "formula node6")

(send node2 insert-izq node5)
(send node2 insert-der node6)

                ;*
        ;1             ;2
    ;3     ;4     ;5        ;6


;(post-orden root)
(pre-orden root)
;(send (send root get-node-izq) print-nodo)