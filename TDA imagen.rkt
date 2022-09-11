#lang racket

;TDA imagen



;-----------------------------------------------------------------------------------
;Definicion de elementos que tendra el TDA
;Elementos que tiene que tener una imagen (tipo largo ancho [pixeles])
;-----------------------------------------------------------------------------------

;Constructor
(define (imagen largo ancho lst-pixeles)
  (list largo ancho lst-pixeles))

;Cosntructor diamico de imagen
(define (imagDin largo ancho . args)
  (list largo ancho args))
;El ". args" hace que la lsita sea dinamica
;El resto de argumentos sera la lista de pixeles de la imagen

;-----------------------------------------------------------------------------------
;Definicion de "imagen-prueba" como una imagen de prueba
(define (imagen-prueba largo ancho . args)
  (display args))
;-----------------------------------------------------------------------------------
;Pertenencia

;Selector

;Modificador

;Otras funciones