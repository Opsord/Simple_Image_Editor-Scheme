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
;Verificador de pertenencia al tipo pixHEX
(define (pixhex? pixel)
  (and(and
       (and [eq? "pixhex-d" (car pixel)]                                     ;-> Verifica el tipo
            [and (integer? (cadr pixel)) (integer? (caddr pixel))])          ;-> Tiene posicion)
       (and [string? (cadddr pixel)]                                         ;-> Contenido en hexadecimal
            [integer? (car(reverse pixel))]) )                               ;-> depth es un numero
      [and (= (length pixel) 5)                                              ;-> Que sea largo 5
           (string? (cadr(reverse pixel)))] ))                               ;-> Que el termino hex sea un string
;-----------------------------------------------------------------------------------
;Pixeles de prueba
;(define pixhex-prueba(pixhex-d 1 0 "#FF0012" 10))
;Veriicacion de prueba
;(pixhex? pixhex-prueba)