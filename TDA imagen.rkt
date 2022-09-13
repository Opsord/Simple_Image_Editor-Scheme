#lang racket

;TDA imagen

;Falta incluir el TDA pixel


;-----------------------------------------------------------------------------------
;Definicion de elementos que tendra el TDA
;Elementos que tiene que tener una imagen (tipo largo ancho [pixeles])
;-----------------------------------------------------------------------------------

;Constructor
(define (imagen largo ancho lst-pixeles)
  (list largo ancho lst-pixeles))

;Cosntructor diamico de imagen
(define (imagDin largo ancho . pixeles) pixeles)
;El ". pixele" hace que la lsita sea dinamica

;-----------------------------------------------------------------------------------

;Falta definir o lograr el tema del tipo de pixel

;Definicion de "imagPrueba" como una imagen de prueba
(define imagPrueba(imagDin 2 2
                           (pixrgb-d 0 0 10 10 10 10)
                           (pixrgb-d 0 1 20 20 20 20)
                           (pixrgb-d 1 0 30 30 30 30)
                           (pixrgb-d 1 1 40 40 40 40)))
;-----------------------------------------------------------------------------------
;Pertenencia

;Selector

;Modificador

;Otras funciones