#lang racket

;TDA imagen

;Elementos que tiene un pixel (posX posY R G B profundidad)
;Elementos que tiene que tener una imagen (tipo largo ancho [pixeles])

(define (pixel posX posY R G B profundidad)
  (list posX posY R G B profundidad))

;El ". args" hace que la lsita sea dinamica
;Cosntructor diamico de imagen
(define (imagen tipo largo ancho . args)
  (list tipo largo ancho args))

;Definicion de "iimagen-prueba" como una imagen de prueba
(define (imagen-prueba tipo largo ancho . args)
  (display args))

;Constructor

;Pertenencia

;Selector

;Modificador

;Otras funciones