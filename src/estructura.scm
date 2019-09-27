#lang scheme


;                 ESTRUCTURA DE LA SOLUCIÓN ENCONTRADA                          
;Raiz del ábol
(define raiz%
   (class object%
      (init-field deduccion)
     
      (field (izq '()) (der '()))
     
      (define/public (insert-izq node)
         (set! izq (cons node izq)))

      (define/public (get-node-izq)
         (car izq))
     
      (super-new)))

;Nodos del árbol (herencia)
(define nodo%
   (class raiz%
      (inherit-field deduccion)
     
      (field (formula null) (regla null))

      (define/public (set-formula nueva-formula)
         (set! formula nueva-formula))

      (define/public (set-regla nueva-regla)
         (set! regla nueva-regla))

      (define/public (to-string)
         (if (and (not (null? formula))(not (null? regla)))
             (string-append "Nodo : " deduccion "   |-" formula "   |-"regla)
             deduccion))
     
      (define/public (print-nodo)
         (define nodo-completo (send this to-string))
         (printf nodo-completo))
      (super-new)))

;(define root(new raiz% (deduccion "deduccion de raiz")))
;(define node1 (new nodo% (deduccion "deduccion de nodo1")))
;(send node1 set-formula "formula nodo1")
;(send node1 set-regla "regla nodo1")
;(define node2 (new nodo% (deduccion "deduccion de nodo2")))
;(send node2 set-formula "formula nodo2")
;(send node2 set-regla "formula nodo2")
;(send root insert-izq node1)
;(send node1 insert-izq node2)
;(send (send root get-node-izq) print-nodo)