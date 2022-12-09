#lang racket

;TDA pixhex-d
;-----------------------------------------------------------------------------------
;Exportacion de funciones
(provide(all-defined-out))
;-----------------------------------------------------------------------------------
;Constructor del tipo Hexadecimal
(define (pixhex-d posX posY HEX depth)
  (list "pixhex-d" posX posY HEX depth))
;-----------------------------------------------------------------------------------
;Pertenencia

;; Descripción: Constructor de pixel tipo hexadecimal
;; Tipo de algoritmo/estrategia: Constructor dinamico de una lista para n-elementos
;; Tipo de recursión: NA
;; Dom: numero X numero X hexadecimal X numero
;; Rec: pixelHEX
(define (pixhex? pixel)
  (and(and
       (and [eq? "pixhex-d" (car pixel)]                                     ;-> Verifica el tipo
            [and (integer? (cadr pixel)) (integer? (caddr pixel))])          ;-> Tiene posicion)
       (and [string? (cadddr pixel)]                                         ;-> Contenido en hexadecimal
            [integer? (cadr(cdddr pixel))]) )                                ;-> depth es un numero
      [= (length pixel) 5]))                                                 ;-> Que sea largo 5
;-----------------------------------------------------------------------------------
;Pixeles de prueba
;(define pixhex-prueba(pixhex-d 1 0 "#FF0012" 10))
;Veriicacion de prueba
;(pixhex? pixhex-prueba)