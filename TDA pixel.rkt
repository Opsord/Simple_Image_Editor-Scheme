#lang racket

;TDA pixel



;-----------------------------------------------------------------------------------
;Definicion de elementos que tendra el TDA
;Elementos que tiene un pixel (posX posY R G B profundidad)
;Se definen los 3 tipos de pixeles que hay
;-----------------------------------------------------------------------------------
;Constructores

;Constructor del tipo Bit
(define (pixbit-d posX posY bool depth)
  (list posX posY bool depth))

;Constructor del tipo RGB
;R G B tiene que ser entre 0 y 255 (implementar en pertenencia)
(define (pixrgb-d posX posY R G B depth)
  (list posX posY R G B depth))

;Constructor del tipo Hexadecimal
(define (pixhex-d posX posY HEX depth)
  (list posX posY HEX depth))

;-----------------------------------------------------------------------------------
;Pertenencia

;Selector

;Modificador

;Otras funciones