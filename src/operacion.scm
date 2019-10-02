#lang scheme

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

(define (implicaIzquierda operacion)
   (define opTemp
         (new Operacion% (operandos (list (car (send operacion get-operandos))))
              (operador "~")
              (zona (send operacion get-zona))
         )
    )

  (define opTemp2
        (new Operacion% (operandos (list opTemp (cdr (send operacion get-operandos))))
             (operador "v")
             (zona (send operacion get-zona))
        )
   )

  opTemp2
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

(set! premisa (cons operacion premisa))

;============================================================
;============================================================

(set! operacion
  (new Operacion% (operandos '("q"))
       (operador "~")
       (zona "conclusion")
  )
)

(define operacion2
  (new Operacion% (operandos '("p"))
       (operador "~")
       (zona "conclusion")
  )
)

(define operacion3
  (new Operacion% (operandos '(operacion operacion2))
       (operador "->")
       (zona "conclusion")
  )
)

(set! conclusion (cons operacion3 conclusion))


;============================================================
; ((~ p) v q) => ((~ q) -> (~ p))
;============================================================

(set! premisa (cons (implicaIzquierda (car premisa)) premisa))

(send (car premisa) get-operandos)
(send (car premisa) get-operador)
(send (car premisa) get-zona)

;============================================================
; ((~ p) v q) => (q v (~ p))
;============================================================

(set! premisa (cons (implicaIzquierda (car premisa)) premisa))

(send (car premisa) get-operandos)
(send (car premisa) get-operador)
(send (car premisa) get-zona)

;===========================================

(define (recorrerOperacion operacion)
  (declare operandos (send operacion get-operandos))
  (cond
    ((not (string? (car operandos)))
      (recorrerOperacion (car operandos))
    )

    ((not (string? (cdr operandos)))
      (recorrerOperacion (cdr operandos))
    )

    (else

     
    )
  )
)