#lang racket

;TDA pixel



;-----------------------------------------------------------------------------------
;Definicion de elementos que tendra el TDA
;Elementos que tiene un pixel (posX posY R G B profundidad)
;-----------------------------------------------------------------------------------

;Constructor
(define (pixel tipo posX posY R G B profundidad)
  (list tipo posX posY R G B profundidad))

;-----------------------------------------------------------------------------------
;Pertenencia

;Selector

;Modificador

;Otras funciones