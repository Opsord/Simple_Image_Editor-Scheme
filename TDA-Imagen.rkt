#lang racket

;TDA imagen

;Importacion de TDA pixel
(require "TDA-Pixel.rkt")

;-----------------------------------------------------------------------------------
;Constructor diamico de imagen

;; Descripción: Constructor dinamico de una imagen
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: numero X numero X n-pieles
;; Rec: imagen
(define (image largo ancho . pixeles) pixeles)
;El ". pixel" hace que la lsita sea dinamica

;-----------------------------------------------------------------------------------

;Definicion de "imagen-prueba" como una imagen de prueba
(define imagen-prueba(image 2 2
                           (pixrgb-d 0 0 10 10 10 10)
                           (pixrgb-d 0 1 20 20 20 20)
                           (pixrgb-d 1 0 30 30 30 30)
                           (pixrgb-d 1 1 40 40 40 40)))
;-----------------------------------------------------------------------------------
;Pertenencia
;-----------------------------------------------------------------------------------
;Selector
;-----------------------------------------------------------------------------------
;Modificador
;-----------------------------------------------------------------------------------
;Otras funciones
;-----------------------------------------------------------------------------------