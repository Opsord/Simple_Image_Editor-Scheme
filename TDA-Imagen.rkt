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

;bitmap?
(define (bitmap? image)
  (define (bitmap-inner image acumulador)
    (if (null? (cdr image))
        acumulador
        (bitmap-inner (cdr image) (and (pixbit? (car image)) acumulador))))
  (bitmap-inner image #t))

;pixmap?
(define (pixmap? image)
  (define (pixmap-inner image acumulador)
    (if (null? (cdr image))
        acumulador
        (pixmap-inner (cdr image) (and (pixrgb? (car image)) acumulador))))
  (pixmap-inner image #t))

;hexmap?
(define (hexmap? image)
  (define (hexmap-inner image acumulador)
    (if (null? (cdr image))
        acumulador
        (hexmap-inner (cdr image) (and (pixhex? (car image)) acumulador))))
  (hexmap-inner image #t))

;-----------------------------------------------------------------------------------
;Selector
;-----------------------------------------------------------------------------------
;Modificador
;-----------------------------------------------------------------------------------
;Otras funciones
;-----------------------------------------------------------------------------------