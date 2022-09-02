#lang racket

;TDA imagen

;Elementos que tiene un pixel (posX posY R G B profundidad)
;Elementos que tiene que tener una imagen (tipo largo ancho [pixeles])

(define (pixel posX posY R G B profundidad)
  (list posX posY R G B profundidad))

;El ". args" hace que la lsita sea dinamica

(define (imagen tipo largo ancho . args)
  (list tipo largo ancho))

;Constructor

;Pertenencia

;Selector

;Modificador

;Otras funciones