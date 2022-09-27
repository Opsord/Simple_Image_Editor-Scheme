#lang racket

;TDA pixrgb-d
;-----------------------------------------------------------------------------------
;Exportacion de funciones
(provide(all-defined-out))
;-----------------------------------------------------------------------------------
;Constructor del tipo RGB
(define (pixrgb-d posX posY R G B depth)
  (list "pixrgb-d" posX posY R G B depth))
;-----------------------------------------------------------------------------------
;Pertenencia

;; Descripción: Constructor de pixel tipo RGB
;; Tipo de algoritmo/estrategia: Constructor dinamico de una lista para n-elementos
;; Tipo de recursión: NA
;; Dom: numero X numero X numero X nummero X numero X numero
;; Rec: pixelRGB
(define (pixrgb? pixel)
  (and (and 
        (and [eq? "pixrgb-d" (car pixel)]                                             ;-> Verifica el tipo
             [and (integer? (cadr pixel)) (integer? (caddr pixel))]                   ;-> Tiene posicion
             )
        (and [and (>= (cadddr pixel) 0) (<= (cadddr pixel) 255)]                      ;-> R entre 0 y 255
             [and (>= (cadddr (cdr pixel)) 0) (<= (cadddr (cdr pixel)) 255)]          ;-> G entre 0 y 255
             ))
       (and [and (>= (cadddr (cdr(cdr pixel))) 0) (<= (cadddr (cdr(cdr pixel))) 255)] ;-> B entre 0 y 255
            [integer? (car(reverse pixel))]                                           ;-> Verifica la profundidad
            )))
;-----------------------------------------------------------------------------------
;Pixeles de prueba
;(define pixrgb-prueba(pixrgb-d 1 1 0 100 255 10))
;Verificacion de prueba
;(pixrgb? pixrgb-prueba)