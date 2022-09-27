#lang racket

;TDA pixbit-d
;-----------------------------------------------------------------------------------
;Exportacion de funciones
(provide(all-defined-out))
;-----------------------------------------------------------------------------------
;Constructor del tipo Bit
(define (pixbit-d posX posY bool depth)
  (list "pixbit-d" posX posY bool depth))
;-----------------------------------------------------------------------------------
;Pertenencia

;; Descripción: Constructor de pixel tipo BIT
;; Tipo de algoritmo/estrategia: Constructor dinamico de una lista para n-elementos
;; Tipo de recursión: NA
;; Dom: numero X numero X numero X numero
;; Rec: pixelBIT
(define (pixbit? pixel)
  (and (and
        (and [eq? "pixbit-d" (car pixel)]                             ;-> Verifica el tipo
             [and (integer? (cadr pixel)) (integer? (caddr pixel))]   ;-> Verifica posicion
             )
        (and [or (= (cadddr pixel) 0) (= (cadddr pixel) 1)]           ;-> Contenido del pixel es 0 o 1
             [integer? (car(reverse pixel))]                          ;-> Verifica la profundidad
             ))
       (= (length pixel) 5)))
;-----------------------------------------------------------------------------------
;Pixeles de prueba
;(define pixbit-prueba(pixbit-d 1 1 0 10))
;Verificaciones de prueba
;(pixbit? pixbit-prueba)